{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import           CodeSample                 hiding (defaultMain)
import qualified MainWithConduit

import           Control.Lens               ((^.))
import qualified Control.Monad              as M
import qualified Data.Attoparsec.ByteString as PB
import           Data.Bool                  (bool)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import qualified Data.Conduit.Combinators   as CC
import qualified Data.Conduit.List          as CList
import           Data.Either                (either, isLeft, isRight)
import           Data.Fixed
import           Data.Function              (on)
import           Data.Functor.Identity
import qualified Data.List                  as L
import           Data.Monoid                (Sum (..))
import           Data.Ratio
import           Data.Sort                  (sortOn)
import           Data.Tagged
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Time.Calendar         (fromGregorian)
import           Data.Time.LocalTime
import           Data.Vinyl.Lens            (rlens)
import           Data.Word                  (Word8)
import qualified Money
import           Network.Pcap.Conduit       (Packet)
import qualified Refined                    as R
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain $ testGroup "Tests" [unitTests, quickCheckTests]

dg :: Word8 -> Digit
dg = Digit . either (const $ error "Impossible digit") id . R.refine

switcheroo :: Show a => a -> String
switcheroo s = "Should be valid: " ++ show s

errMsg :: (Show a, Show err) => a -> err -> String
errMsg v msg = show msg ++ " in " ++ show v

commonParse :: PB.Parser a -> B.ByteString -> Either String a
commonParse p = PB.parseOnly (p <* PB.endOfInput)

propParsePktIssueCode :: Tagged "PktIssueCode" B.ByteString -> Bool
propParsePktIssueCode =
  let parse' = commonParse quotePktIssueCode in isRight . parse' . unTagged

propParsePktPrice :: Tagged "PktPrice" B.ByteString -> Bool
propParsePktPrice =
  let parse' = commonParse quotePktPrice in isRight . parse' . unTagged

propParsePktQty :: Tagged "PktQuantity" B.ByteString -> Bool
propParsePktQty =
  let parse' = commonParse quotePktQty in isRight . parse' . unTagged

propParsePktQuoteAcceptTime :: Tagged "PktTod" B.ByteString -> Bool
propParsePktQuoteAcceptTime =
  let parse' = commonParse quotePktQuoteAcceptTime
  in  isRight . parse' . unTagged

propParseQuoteSummary :: Tagged "QuoteSummaryPkt" Packet -> Bool
propParseQuoteSummary = isRight . parseQuoteSummary'

commonPropMain'
  :: Tagged "sort?" Bool -> [Tagged "QuoteSummaryPkt" Packet] -> Bool
commonPropMain' (Tagged shouldSort) taggedPkts =
  let condSort = bool id (sortOn (^. rlens @'QuoteAcceptTime)) shouldSort

      sortedListEq =
          let group' = L.groupBy ((==) `on` unAttrRecQuoteAcceptTime')
              eq'    = fmap (== []) . (L.\\)
          in  fmap (L.all (== True)) . (L.zipWith eq' `on` group')

      condSortedListEq = bool (==) sortedListEq shouldSort

      quotesSummarys =
          condSort
            . either (const $ error "Invalid gen Quote Summary packets") id
            $ traverse parseQuoteSummary' taggedPkts
      conduitQuotesSummarys =
          runIdentity . MainWithConduit.interpret $ MainWithConduit.MainF
            (const $ CList.sourceList . fmap unTagged $ taggedPkts)
            (CC.foldMap $ pure @[])
            (Options shouldSort Nothing)

  in  conduitQuotesSummarys `condSortedListEq` quotesSummarys

propMainNotGobblingPkts' :: [Tagged "QuoteSummaryPkt" Packet] -> Bool
propMainNotGobblingPkts' = commonPropMain' (Tagged @"sort?" False)

propMainSortingCorrectly' :: [Tagged "QuoteSummaryPkt" Packet] -> Bool
propMainSortingCorrectly' = commonPropMain' (Tagged @"sort?" True)

{-
 - I didn't use brittany (automatic code formatter) from here on
 - as it didn't make for legible tests
 -}

quickCheckTests :: TestTree
quickCheckTests = testGroup
  "(checked by QuickCheck)"
  [ testProperty "Parse quote packet Issue Code" $
      withMaxSuccess 250 $
        forAll genPktIssueCode propParsePktIssueCode
  , testProperty "Parse quote packet Price" $
      withMaxSuccess 600 $
        forAll genPktPrice propParsePktPrice
  , testProperty "Parse quote packet Quantity" $
      withMaxSuccess 600 $
        forAll genPktQty propParsePktQty
  , testProperty "Parse quote packet Quote accept time" $
      withMaxSuccess 250 $
        forAll (mkPktTod' <$> genTod) propParsePktQuoteAcceptTime
  , testProperty "Parse Quote Summary packet" $
      withMaxSuccess 400 $
        forAll genQuoteSummaryPkt propParseQuoteSummary
  , testGroup "Main is, apparently, not gobbling packets" [
      testProperty "50-300 packets" $
        withMaxSuccess 10 $
          forAllBlind
            (choose (50, 300) >>= flip M.replicateM genQuoteSummaryPkt)
            propMainNotGobblingPkts'

    , testProperty "300-1500 packets" $
        withMaxSuccess 10 $
          forAllBlind
            (choose (300, 1500) >>= flip M.replicateM genQuoteSummaryPkt)
            propMainNotGobblingPkts'

    , testProperty "3000 packets" $
        withMaxSuccess 3 $
          forAllBlind (M.replicateM 3000 genQuoteSummaryPkt)
                      propMainNotGobblingPkts'
    ]

  , testGroup "Main is, apparently, sorting correctly" $
      let prop' maxSuccess t (r0, r1) =
            testProperty
              (L.concat [show t, "s with ", show r0, "-", show r1, " packets"])
              $ withMaxSuccess maxSuccess $
                  forAllBlind (genAdjPkts' t (r0, r1))
                              propMainSortingCorrectly'
      in [
        prop' 100 1  (1, 5)
      , prop' 100 3  (1, 5)
      , prop' 100 6  (1, 5)
      , prop' 100 7  (1, 5)
      , prop' 100 9  (1, 5)
      , prop' 100 11 (1, 5)
      , prop' 100 15 (1, 5)

      , prop' 100 4  (100, 150)
      , prop' 100 7  (100, 150)
      , prop' 100 11 (100, 150)

      , prop' 50 15 (15, 20)

      , prop' 10 30 (400, 600)
      ]
  ]

unitTests :: TestTree
unitTests = testGroup
  "Unit tests"
  [ testCase "`base36` trivial" $ do
      base36ToBase10 '0' @?= Just 0
      base36ToBase10 '3' @?= Just 3
      base36ToBase10 '9' @?= Just 9
      base36ToBase10 'A' @?= Just 10
      base36ToBase10 'L' @?= Just 21
      base36ToBase10 'Z' @?= Just 35
      (base36ToBase10 'a' == Nothing) @? "No lowercase"
      (base36ToBase10 ' ' == Nothing) @? "No symbols"

  , testCase "Luhn validation trivial" $ do
      let isValid s = isValidLuhnChecksum $ Tagged @"LuhnChecksum" (Sum @Word8 s)

      isValid       0  @? "0 is a valid Luhn checksum"
      not (isValid  1) @? "1 is not a valid Luhn checksum"
      isValid      10  @? "10 is a valid Luhn checksum"
      not (isValid 11) @? "11 is not a valid Luhn checksum"

  , testCase "Luhn checksum" $ do
      let luhn' = luhnChecksum . fmap dg

      luhn' [] @?= 0
      luhn' [1] @?= 1
      luhn' [7] @?= 7
      luhn' [3,8,5] @?= 15
      luhn' [4,9,1,6,8,2,2,0,2,0,0,5,7,5,9,4] @?= 70
      luhn' [4,4,8,5,3,0,4,9,4,9,9,1,1,9,8,9,1,5,0] @?= 90
      luhn' [6,7,6,2,4,9,1,4,5,1,3,7,1,9,2,7] @?= 75
      luhn' [4,9,9,2,7,3,9,8,7,1,7,3] @?= 67
      luhn' [1,6,5,2,3,1,8,4,2,9,1,9,8,2,2,7,1,5,2,7,2,4,6,1,4] @?= 105
      luhn' [2,5,2,1,0,9,1,3,5,2,5,4,4,1,1,1,8,4,4,1,6,0,1,7,3
            ,3,9,1,0,7,1,2,4,6,5,2,1,3,2,0,0,1,1,0,1,1,3,1,5,0
            ,1,9,3,5,6,4,1,3,1,2,1,1,1,6,4,5,7,1,2,4,1,9,0,6,5]
        @?= 248
      -- KROMT9NF3KM7
      luhn' [2,0,2,7,2,4,2,2,2,9,9,2,3,1,5,3,2,0,2,2,7] @?= 80
      -- BRPOK76O49O3
      luhn' [1,1,2,7,2,5,2,4,2,0,7,6,2,4,4,9,2,4,3] @?= 71

  , testCase "Luhn check digit" $ do
      let checkDigit = luhnCheckDigit' . Tagged @"...0" . fmap dg

      checkDigit [] @?= 0
      checkDigit [1,0] @?= 8
      checkDigit [7,0] @?= 5
      checkDigit [3,8,0] @?= 0
      checkDigit [1,2,3,4,5,6,7,8,1,2,3,4,5,6,7,0] @?= 0
      checkDigit [4,9,1,6,8,2,2,0,2,0,0,5,7,5,9,0] @?= 4
      checkDigit [1,6,5,2,3,1,8,4,2,9,1,9,8,2,2,7,1,5,2,7,2,4,6,1,0] @?= 9
      checkDigit [2,5,2,1,0,9,1,3,5,2,5,4,4,1,1,1,8,4,4,1,6,0,1,7,3
                 ,3,9,1,0,7,1,2,4,6,5,2,1,3,2,0,0,1,1,0,1,1,3,1,5,0
                 ,1,9,3,5,6,4,1,3,1,2,1,1,1,6,4,5,7,1,2,4,1,9,0,6,0]
        @?= 7
      -- KROMT9NF3KM7
      checkDigit [2,0,2,7,2,4,2,2,2,9,9,2,3,1,5,3,2,0,2,2,0] @?= 7
      -- KRLQL75WU0Y0
      checkDigit [2,0,2,7,2,1,2,6,2,1,7,5,3,2,3,0,0,3,4,0] @?= 0
      -- USNZOKPEV3J4
      checkDigit [3,0,2,8,2,3,3,5,2,4,2,0,2,5,1,4,3,1,3,1,9,0] @?= 4

  , testCase "Country code validation" $ do
      let ref = R.refine @ValidCountryCodeCanon
          h f s =
            let r = ref s
            in  f r @? either (errMsg s) switcheroo r

          valid :: Text -> Assertion
          valid = h isRight
          invalid :: Text -> Assertion
          invalid = h isLeft

      invalid "" >> invalid "a" >> invalid "ab" >> invalid "ABC"
      invalid "XZ" >> invalid "EU" >> invalid "EQ" >> invalid "dce"
      valid   "JP" >> invalid "JPN"
      valid   "KR" >> invalid "KOR"
      valid   "BR" >> invalid "BRA"
      valid   "ZW" >> valid   "YE" >> valid   "LY" >> valid   "EE"

  , testCase "`isinDigits`" $ do
      isinDigits (Isin "ZW") @?= Just (fmap dg [3,5,3,2])
      isinDigits (Isin "YE") @?= Just (fmap dg [3,4,1,4])
      isinDigits (Isin "KRFGO6K2JXT1")
        @?= Just (fmap dg [2,0,2,7,1,5,1,6,2,4,6,2,0,2,1,9,3,3,2,9,1])
      isinDigits (Isin "JP4GUHW06TF1")
        @?= Just (fmap dg [1,9,2,5,4,1,6,3,0,1,7,3,2,0,6,2,9,1,5,1])
      isinDigits (Isin "JPE3PTAA0R97")
        @?= Just (fmap dg [1,9,2,5,1,4,3,2,5,2,9,1,0,1,0,0,2,7,9,7])
      isinDigits (Isin "US6VYTBWKWU3")
        @?= Just (fmap dg [3,0,2,8,6,3,1,3,4,2,9,1,1,3,2,2,0,3,2,3,0,3])
      isinDigits (Isin "NZ2KZNTQQ9C0")
        @?= Just (fmap dg [2,3,3,5,2,2,0,3,5,2,3,2,9,2,6,2,6,9,1,2,0])
      isinDigits (Isin "PN451CU9N6V8")
        @?= Just (fmap dg [2,5,2,3,4,5,1,1,2,3,0,9,2,3,6,3,1,8])

  , testCase "Isin validation" $ do
      let ref = R.refine @ValidIsin
          h f s =
            let r = ref (Isin s)
            in  f r @? either (errMsg s) switcheroo r

          valid :: Text -> Assertion
          valid = h isRight
          invalid :: Text -> Assertion
          invalid = h isLeft

      invalid "123"           -- length
      invalid "1234567890123" -- length
      invalid "usTO99TD39W5"  -- case
      invalid "US07z5ETQ0S9"  -- case
      invalid "BR4C7-JD5R33"  -- symbol
      invalid "AX07Z5E$Q0S9"  -- symbol
      invalid "ZDJNHM1YC5T2"  -- country code
      invalid "NZ1S7GF8JZQ6"  -- Luhn
      invalid "JP39YXOD99W7"  -- Luhn
      invalid "KRUBF31A9538"  -- Luhn
      valid   "KR0H5POQFMV0"
      valid   "CVL13A8H1678"
      valid   "KRCNR2ANIWF3"
      valid   "KRCXAUHEUGX2"
      valid   "JPLCMJ7FXZM8"
      valid   "JPYD0BHLH3T6"
      valid   "YE9XS3FSS6T9"
      valid   "USIAZKR8JT20"

  , testCase "Time of day to milliseconds" $ do
      todToMilli (TimeOfDay 0  0 0) @?= 0
      todToMilli (TimeOfDay 12 0 0) @?= 43200
      todToMilli (TimeOfDay 5  0 0) @?= 18000
      todToMilli (TimeOfDay 0  7 0) @?= 420
      todToMilli (TimeOfDay 0  0 2) @?= 2
      todToMilli (TimeOfDay 0  0 $ MkFixed @E12 $ timesTenToThe 9 999)
        @?= (MkFixed @E3 999)
      todToMilli (TimeOfDay 9  17 $ MkFixed @E12 46174947596251)
        @?= (MkFixed @E3 33466174)
      todToMilli (TimeOfDay 2  55 $ MkFixed @E12 19164547183930)
        @?= (MkFixed @E3 10519164)
      todToMilli (TimeOfDay 5  2  $ MkFixed @E12 15453990711868)
        @?= (MkFixed @E3 18135453)
      todToMilli (TimeOfDay 14 8  $ MkFixed @E12 52678158707759)
        @?= (MkFixed @E3 50932678)
      todToMilli (TimeOfDay 15 52 $ MkFixed @E12 31402490610645)
        @?= (MkFixed @E3 57151402)

  , testCase "Display padded time of day milliseconds" $ do
      let display = displayPaddedTodMs'

      display (TimeOfDay 0  0 0) @?= "000"
      display (TimeOfDay 20 53 $ MkFixed @E12 55010013063405) @?= "010"
      display (TimeOfDay 4  3  $ MkFixed @E12 43100592625988) @?= "100"
      display (TimeOfDay 4  10 $ MkFixed @E12 40008182014198) @?= "008"
      display (TimeOfDay 14 25 $ MkFixed @E12 12003023314918) @?= "003"
      display (TimeOfDay 16 47 $ MkFixed @E12 019271744086)   @?= "019"
      display (TimeOfDay 20 41 $ MkFixed @E12 877176557876)   @?= "877"

  , testCase "Time of day to packet time of day" $ do
      mkPktTod' (TimeOfDay 0  0  0)  @?= "00000000"
      mkPktTod' (TimeOfDay 12 0  0)  @?= "12000000"
      mkPktTod' (TimeOfDay 1  5  9)  @?= "01050900"
      mkPktTod' (TimeOfDay 15 14 29) @?= "15142900"
      mkPktTod' (TimeOfDay 16 57 $ MkFixed @E12 43034450551930) @?= "16574303"
      mkPktTod' (TimeOfDay 11 54 $ MkFixed @E12 17556452396098) @?= "11541755"
      mkPktTod' (TimeOfDay 16 05 $ MkFixed @E12 8322367043037) @?= "16050832"
      mkPktTod' (TimeOfDay 5  22 $ MkFixed @E12 26993450551930) @?= "05222699"

  , testCase "`addLocalTime'`" $ do
      let add i = addLocalTime' (MkFixed @E12 i)
          arbDay = fromGregorian 1997 7 23
          localTime' = LocalTime arbDay

      add (timesTenToThe 9  137) (localTime' $ TimeOfDay 0 0 0)
        @?= (localTime' $ TimeOfDay 0 0 $ MkFixed @E12 $ timesTenToThe 9 137)
      add (timesTenToThe 12 23) (localTime' $ TimeOfDay 0 0 0)
        @?= (localTime' $ TimeOfDay 0 0 23)
      add 999402645673518245
        (localTime' $ TimeOfDay 15 35 $ MkFixed @E12 44949652232613)
          @?= (LocalTime (fromGregorian 1997 8 4)
                         (TimeOfDay 5 12 $ MkFixed @E12 27595325750858))
      add 48166342532652
        (localTime' $ TimeOfDay 17 55 $ MkFixed @E12 20077771954426)
          @?= localTime' (TimeOfDay 17 56 $ MkFixed @E12 8244114487078)
      add 9387465482610107
        (localTime' $ TimeOfDay 11 23 $ MkFixed @E12 37691252455656)
          @?= localTime' (TimeOfDay 14 0 $ MkFixed @E12 5156735065763)
      add 322607922637619524
        (LocalTime (fromGregorian 2011 27 4)
                   (TimeOfDay 14 23 $ MkFixed @E12 52112491395045))
          @?= (LocalTime (fromGregorian 2011 12 8)
                         (TimeOfDay 8 0 $ MkFixed @E12 40035129014569))
      add 69355879833215133659386
        (LocalTime (fromGregorian 2013 10 29)
                   (TimeOfDay 22 8 $ MkFixed @E12 36519733351663))
          @?= (LocalTime (fromGregorian 4211 8 19)
                         (TimeOfDay 0 19 $ MkFixed @E12 9734867011049))

  , testCase "Display price" $ do
      displayPrice' 0 @?= "0.00"
      displayPrice' 10 @?= "10.00"
      displayPrice' (Money.dense' $ 1 % 100) @?= "0.01"
      displayPrice' (Money.dense' $ 51504529984 % 11906145451) @?= "4.33"
      displayPrice' (Money.dense' $ 11519672665473 % 1450253) @?= "7943215.88"
      displayPrice' (Money.dense' $ 1234567890 % 100) @?= "12345678.90"
      displayPrice' (Money.dense' $ 81260056846217 % 735299188412) @?= "110.51"
      -- this is assumed correct behavior
      displayPrice' (Money.dense' $ 12345) @?= "12345.00"

  , testCase "Display quantity" $ do
      displayQty' 0 @?= "0.0"
      displayQty' 10 @?= "10.0"
      displayQty' (1 % 100) @?= "0.0"
      displayQty' (6201464918908 % 40288523987) @?= "153.9"
      displayQty' (8929139133 % 8715788) @?= "1024.5"
      displayQty' (9377814583330 % 8008081) @?= "1171043.9"
      displayQty' (1234567890 % 100) @?= "12345678.9"
      -- this is assumed correct behavior
      displayQty' 1234567 @?= "1234567.0"

  , testCase "Parse Issue Code" $ do
      let parse' = commonParse quotePktIssueCode
          isin' = either (const $ error "Impossible isin") id . R.refine . Isin
          valid s = parse' (C.pack . T.unpack $ s) @?= Right (isin' s)
          invalid s =
            let r = parse' s
            in isLeft r @? either (errMsg s) switcheroo r

      valid "KRXBLQMYUOB7" >> valid "KRXBLQMYUOB7"
      valid "KRHKLZD3C4Y2" >> valid "KRQTY8MB9XX0"
      valid "USZC20QIFKF8" >> valid "NZB6E8XSR7I4"
      valid "JP15AI8IWUW9" >> valid "JP41QB6JA756"
      valid "JPGV0ZS2D6V7" >> valid "TF7W251M0MA1"

      invalid "123"           -- length
      invalid "1234567890123" -- length
      invalid "usTO99TD39W5"  -- case
      invalid "US07z5ETQ0S9"  -- case
      invalid "BR4C7-JD5R33"  -- symbol
      invalid "AX07Z5E$Q0S9"  -- symbol
      invalid "ZDJNHM1YC5T2"  -- country code
      invalid "NZ1S7GF8JZQ6"  -- Luhn
      invalid "JP39YXOD99W7"  -- Luhn
      invalid "KRUBF31A9538"  -- Luhn

  , testCase "Parse quote packet Price" $ do
      let parse' = commonParse quotePktPrice
          invalid s =
            let r = parse' s
            in isLeft r @? either (errMsg s) switcheroo r

      parse' "00000" @?= Right (Money.dense' 0)
      parse' "00.00" @?= Right (Money.dense' 0)
      parse' "00.01" @?= Right (Money.dense' $ 1 % 100)
      invalid "1234" >> invalid "123456" >> invalid "abcde"
      invalid "1.952" >> invalid ".376" >> invalid "929."
      invalid "1.9" >> invalid ".5" >> invalid "1.99" >> invalid "123a4"
      invalid "a1234" >> invalid "3474a" >> invalid "a123b"
      invalid "123e0" >> invalid "1.1e0"
      invalid "123e1" >> invalid "1.1e1"

      parse' "00007" @?= Right (Money.dense' $ 7)
      parse' "00045" @?= Right (Money.dense' $ 45)
      parse' "00116" @?= Right (Money.dense' $ 116)
      parse' "00754" @?= Right (Money.dense' $ 754)
      parse' "07992" @?= Right (Money.dense' $ 7992)
      parse' "01268" @?= Right (Money.dense' $ 1268)
      parse' "01575" @?= Right (Money.dense' $ 1575)
      parse' "16527" @?= Right (Money.dense' $ 16527)
      parse' "004.7" @?= Right (Money.dense' $ 47 % 10)
      parse' "001.5" @?= Right (Money.dense' $ 15 % 10)
      parse' "31.09" @?= Right (Money.dense' $ 3109 % 100)
      parse' "56.55" @?= Right (Money.dense' $ 1131 % 20)
      parse' "967.4" @?= Right (Money.dense' $ 4837 % 5)
      parse' "340.3" @?= Right (Money.dense' $ 3403 % 10)
      parse' "078.0" @?= Right (Money.dense' $ 78)
      parse' "049.0" @?= Right (Money.dense' $ 49)
      parse' "165.0" @?= Right (Money.dense' $ 165)
      parse' "003.0" @?= Right (Money.dense' $ 3)
      parse' "304.1" @?= Right (Money.dense' $ 3041 % 10)
      parse' "454.3" @?= Right (Money.dense' $ 4543 % 10)

  , testCase "Parse quote packet Quantity" $ do
      let parse' = commonParse quotePktQty
          invalid s =
            let r = parse' s
            in isLeft r @? either (errMsg s) switcheroo r

      parse' "0000000" @?= Right 0
      parse' "00000.0" @?= Right 0
      parse' "00000.1" @?= Right (1 % 10)
      invalid "123456" >> invalid "12345678" >> invalid "abcdefg"
      invalid "133.446" >> invalid ".123456" >> invalid "543112."
      invalid "1335.46" >> invalid ".99" >> invalid "1.98"
      invalid ".4" >> invalid "2.54" >> invalid "1234.5" >> invalid "1234a56"
      invalid "a332641" >> invalid "298721a" >> invalid "a22349b"
      invalid "12345e0" >> invalid "1.123e0"
      invalid "12345e1" >> invalid "1.123e1"

      parse' "0000007" @?= Right 7
      parse' "0000049" @?= Right 49
      parse' "0000090" @?= Right 90
      parse' "0000847" @?= Right 847
      parse' "0000528" @?= Right 528
      parse' "0005899" @?= Right 5899
      parse' "0008340" @?= Right 8340
      parse' "00538.7" @?= Right (5387 % 10)
      parse' "00112.4" @?= Right (562 % 5)
      parse' "00073.0" @?= Right 73
      parse' "00029.0" @?= Right 29
      parse' "00751.0" @?= Right 751
      parse' "00264.0" @?= Right 264
      parse' "00425.5" @?= Right (851 % 2)
      parse' "00649.4" @?= Right (3247 % 5)
      parse' "00649.4" @?= Right (3247 % 5)
      parse' "33501.6" @?= Right (167508 % 5)
      parse' "63238.2" @?= Right (316191 % 5)
      parse' "44994.7" @?= Right (449947 % 10)
      parse' "00917.8" @?= Right (4589 % 5)

  , testCase "Parse quote packet Quote accept time" $ do
      let parse' = commonParse quotePktQuoteAcceptTime
          invalid s =
            let r = parse' s
            in isLeft r @? either (errMsg s) switcheroo r

      invalid "000000" >> invalid "0000000" >> invalid "000000000"
      invalid "2500000" >> invalid "00600000" >> invalid "00006100"
      invalid "a0000000" >> invalid "000000a" >> invalid "aa000000"
      invalid "000000aa" >> invalid "00" >> invalid "0000"

      parse' "00000000" @?= Right (TimeOfDay 0 0 0)
      parse' "12000000" @?= Right (TimeOfDay 12 0 0)
      parse' "00003500" @?= Right (TimeOfDay 0 0 35)
      parse' "00130100" @?= Right (TimeOfDay 0 13 1)
      parse' "00120000" @?= Right (TimeOfDay 0 12 0)
      parse' "00006000" @?= Right (TimeOfDay 0 0 60)   -- to account for
      parse' "03056099" @?=                            --   leap seconds
        Right (TimeOfDay 3 5 $ MkFixed @E12 $ timesTenToThe 9 60990)
      parse' "23595999" @?=
        Right (TimeOfDay 23 59 $ MkFixed @E12 $ timesTenToThe 9 59990)
      parse' "19405075" @?=
        Right (TimeOfDay 19 40 $ MkFixed @E12 $ timesTenToThe 9 50750)
      parse' "11053377" @?=
        Right (TimeOfDay 11 5 $ MkFixed @E12 $ timesTenToThe 9 33770)
      parse' "22443665" @?=
        Right (TimeOfDay 22 44 $ MkFixed @E12 $ timesTenToThe 9 36650)
      parse' "14375989" @?=
        Right (TimeOfDay 14 37 $ MkFixed @E12 $ timesTenToThe 9 59890)
      parse' "09215820" @?=
        Right (TimeOfDay 9 21 $ MkFixed @E12 $ timesTenToThe 9 58200)
      parse' "19152840" @?=
        Right (TimeOfDay 19 15 $ MkFixed @E12 $ timesTenToThe 9 28400)
  ]
