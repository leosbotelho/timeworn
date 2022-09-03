{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module CodeSample where

import           Control.Lens                          ((^.))
import           Control.Lens.TH                       (makeLenses)
import           Control.Monad                         (guard, unless, (>=>))
import qualified Control.Monad                         as M
import qualified Control.Monad.Reader                  as Rdr
import qualified Control.Monad.State.Strict            as S
import           Country                               (alphaTwoUpper,
                                                        decodeAlphaTwo)
import           Country.Identifier                    (koreaRepublicOf)
import qualified Data.Attoparsec.ByteString            as PB
import qualified Data.Attoparsec.ByteString.Char8      as PC
import           Data.Attoparsec.Combinator            ((<?>))
import           Data.Bifunctor                        (bimap)
import           Data.Bool                             (bool)
import qualified Data.ByteString                       as B
import qualified Data.ByteString.Char8                 as C
import           Data.Conduit
import qualified Data.Conduit.Combinators              as CC
import qualified Data.Conduit.Lift                     as CL
import qualified Data.Conduit.List                     as CList
import           Data.Either                           (either)
import           Data.Fixed
import           Data.Function                         (on)
import qualified Data.List                             as L
import           Data.Maybe                            (isJust, isNothing,
                                                        maybe)
import           Data.Monoid                           (Sum (..))
import           Data.Singletons                       (Proxy (..), Sing (..),
                                                        sing)
import           Data.Singletons.TH                    (genSingletons)
import           Data.Singletons.TypeLits
import           Data.Sort                             (sortOn)
import           Data.String                           (IsString (..))
import           Data.Tagged
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Text.Prettyprint.Doc             (Doc, Pretty (..))
import qualified Data.Text.Prettyprint.Doc             as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX                 (utcTimeToPOSIXSeconds)
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Typeable                         (typeOf)
import           Data.Vinyl.Core                       (Rec (..))
import           Data.Vinyl.Lens                       (RElem, rlens)
import           Data.Vinyl.TypeLevel                  (RIndex)
import           Data.Word                             (Word32, Word8)
import           GHC.Natural                           (Natural)
import qualified Money
import qualified Money                                 as CNV (Approximation (..),
                                                               DecimalConf (..),
                                                               defaultDecimalConf,
                                                               denseToDecimal,
                                                               scaleFromRational,
                                                               separatorsDot)
import qualified Money.Internal                        as CNV (rationalToDecimal)
import qualified Network.Pcap                          as Pcap
import           Network.Pcap.Conduit                  (Packet)
import qualified Options.Applicative                   as O
import           Refined                               (Refined, catchRefine, throwRefineOtherException)
import qualified Refined                               as R
import qualified System.IO                             (stdout)
import           Test.QuickCheck                       (Arbitrary (..), Gen)
import qualified Test.QuickCheck                       as QC

data MarketDataField' a b
    = IssueCode
    | BestBid a b
    | BestAsk a b
    | QuoteAcceptTime

genSingletons [''MarketDataField']

type MarketDataField = MarketDataField' Nat Symbol

newtype Isin a = Isin
  { unIsin :: a
  } deriving (Eq, Show)

data ValidCountryCode (s :: Symbol) = ValidCountryCode
type ValidCountryCodeCanon = ValidCountryCode "ISO 3166-1 alpha-2"

validCountryCodePredSymbol :: KnownSymbol s => ValidCountryCode s -> Text
validCountryCodePredSymbol =
  let proxy' :: ValidCountryCode s -> Proxy s
      proxy' = const Proxy
  in  T.pack . symbolVal . proxy'

instance R.Predicate ValidCountryCodeCanon Text where
  validate p =
    unless
      <$> isJust
      .   decodeAlphaTwo
      <*> throwRefineOtherException (typeOf p)
      .   (\x -> "Unknown Code `" <> pretty x <> "`")

instance R.Predicate ValidCountryCodeCanon (Isin Text) where
  validate p = R.validate p . T.take 2 . unIsin

newtype Digit = Digit
  { unDigit :: Refined (R.FromTo 0 9) Word8
  } deriving (Eq, Ord, Show)

instance Arbitrary Digit where
  arbitrary =
    Digit
      .   either (const $ error "Impossible gen digit") id
      .   R.refine
      <$> QC.choose (0, 9)

-- @Rosetta Code
luhnChecksum :: [Digit] -> Tagged "LuhnChecksum" (Sum Word8)
luhnChecksum =
  Tagged
    . foldMap (Sum . uncurry (+) . (`divMod` 10))
    . L.zipWith (*) (cycle [1, 2])
    . L.reverse
    . fmap (R.unrefine . unDigit)

luhnCheckDigit' :: Tagged "...0" [Digit] -> Tagged "LuhnCheckDigit" Word8
luhnCheckDigit' =
  Tagged
    . (`mod` 10)
    . (10 -)
    . (`mod` 10)
    . getSum
    . unTagged
    . luhnChecksum
    . unTagged

isValidLuhnChecksum :: Integral a => Tagged "LuhnChecksum" (Sum a) -> Bool
isValidLuhnChecksum = (== 0) . (`mod` 10) . getSum . unTagged

-- do not mess the order of this
indexedBase36Digits :: [Char]
indexedBase36Digits = ['0' .. '9'] ++ ['A' .. 'Z']

base36ToBase10 :: Char -> Maybe Word8
base36ToBase10 = fmap fromIntegral . flip L.elemIndex indexedBase36Digits

isinDigits :: Isin Text -> Maybe [Digit]
isinDigits =
  let f x =
          let (q, r) = x `divMod` 10
              g      = bool id (h q :) $ q > 0
              h      = fmap Digit . R.refine
          in  g [h r]
  in  traverse base36ToBase10
        .   T.unpack
        .   unIsin
        >=> either (const $ Nothing) Just
        .   sequenceA
        .   L.concatMap f

data ValidIsin

instance R.Predicate ValidIsin (Isin Text) where
  validate p isin =
    let throw'          = throwRefineOtherException (typeOf p)
        isinLen         = T.length . unIsin $ isin
        ds              = isinDigits isin
        validateIsinLen = unless
          (isinLen == 12)
          (throw' $ "Invalid ISIN length `" <> pretty isinLen <> "`")
        validateNumericIsin = unless
          (isJust ds)
          (throw' $ "The ISIN must only contain uppercase letters and digits")
        validateLuhn = unless
          (isJust $ ds >>= guard . isValidLuhnChecksum . luhnChecksum)
          (throw' $ "Invalid ISIN Luhn")
        validateIsinCountry =
            let p' = ValidCountryCode :: ValidCountryCodeCanon
                pSymbol = validCountryCodePredSymbol p'
            in  catchRefine
                  (R.validate p' isin)
                  (  const
                  $  throw'
                  $  "Invalid "
                  <> pretty pSymbol
                  <> " Country Code"
                  )
    in  validateIsinLen
          >> validateNumericIsin
          >> validateIsinCountry
          >> validateLuhn

instance Arbitrary (Refined ValidIsin (Isin Text)) where
  arbitrary = do
    securityCode <-
      fmap T.pack . M.replicateM 9 . QC.elements $ indexedBase36Digits
    let
      country = alphaTwoUpper koreaRepublicOf
      digits =
        maybe (error "Impossible gen isin digits") id
          .  isinDigits
          .  Isin
          $  country
          <> securityCode
          <> "0"
      x =
        T.pack . show . unTagged . luhnCheckDigit' . Tagged $ digits

      isin        = Isin $ T.concat [country, securityCode, x]
      refinedIsin = either (const $ error "Impossible gen isin") id
        $ R.refine @ValidIsin isin

    return refinedIsin

type KospiCcySym = "KRW" -- South Korean won

type CcySym = KospiCcySym

{-
 Notice:
 - Negative prices and quantities are allowed at the type-level
 - Prices are decimals with up to two decimal places
 - Quantities are decimals of up to one decimal place

 This might be reassessed, in view of practical and security concerns
 -}
type family MarketData (f :: MarketDataField) where
  MarketData 'IssueCode = Refined ValidIsin (Isin Text)
  MarketData ('BestBid n "Price") = Money.Dense CcySym
  MarketData ('BestBid n "Quantity") = Rational
  MarketData ('BestAsk n "Price") = Money.Dense CcySym
  MarketData ('BestAsk n "Quantity") = Rational
  MarketData 'QuoteAcceptTime = TimeOfDay

newtype MarketDataAttr f = MarketDataAttr
  { unMarketDataAttr :: MarketData f
  }

makeLenses ''MarketDataAttr

mDataEq'
  :: Eq (MarketData f) => MarketDataAttr f -> MarketDataAttr f -> Bool
mDataEq' = on (==) unMarketDataAttr

instance Eq (MarketDataAttr 'IssueCode) where (==) = mDataEq'
instance Eq (MarketDataAttr ('BestBid n "Price")) where (==) = mDataEq'
instance Eq (MarketDataAttr ('BestBid n "Quantity")) where (==) = mDataEq'
instance Eq (MarketDataAttr ('BestAsk n "Price")) where (==) = mDataEq'
instance Eq (MarketDataAttr ('BestAsk n "Quantity")) where (==) = mDataEq'
instance Eq (MarketDataAttr 'QuoteAcceptTime) where (==) = mDataEq'

instance Ord (MarketDataAttr 'QuoteAcceptTime) where
  compare = compare `on` unMarketDataAttr

unAttrRecQuoteAcceptTime'
  :: RElem 'QuoteAcceptTime attrs (RIndex 'QuoteAcceptTime attrs)
  => Rec MarketDataAttr attrs
  -> MarketData 'QuoteAcceptTime
unAttrRecQuoteAcceptTime' = unMarketDataAttr . (^. rlens @'QuoteAcceptTime)

infix 8 =::
(=::) :: Sing f -> MarketData f -> MarketDataAttr f
(=::) = const MarketDataAttr

infix 8 $=::
($=::) :: Functor g => Sing f -> g (MarketData f) -> g (MarketDataAttr f)
($=::) = fmap . (=::)

infixr 3 <*&>
(<*&>) :: Applicative f => f (a r) -> f (Rec a rs) -> f (Rec a (r : rs))
(<*&>) fa ga = (:&) <$> fa <*> ga

type QuoteSummaryFields =
  [ 'IssueCode
  , ('BestBid 1 "Price")
  , ('BestBid 1 "Quantity")
  , ('BestBid 2 "Price")
  , ('BestBid 2 "Quantity")
  , ('BestBid 3 "Price")
  , ('BestBid 3 "Quantity")
  , ('BestBid 4 "Price")
  , ('BestBid 4 "Quantity")
  , ('BestBid 5 "Price")
  , ('BestBid 5 "Quantity")
  , ('BestAsk 1 "Price")
  , ('BestAsk 1 "Quantity")
  , ('BestAsk 2 "Price")
  , ('BestAsk 2 "Quantity")
  , ('BestAsk 3 "Price")
  , ('BestAsk 3 "Quantity")
  , ('BestAsk 4 "Price")
  , ('BestAsk 4 "Quantity")
  , ('BestAsk 5 "Price")
  , ('BestAsk 5 "Quantity")
  , 'QuoteAcceptTime
  ]

type QuoteSummary = Rec MarketDataAttr QuoteSummaryFields

bestBidPriceArb :: SNat n -> Gen (MarketDataAttr ( 'BestBid n "Price"))
bestBidPriceArb n = SBestBid n sing $=:: fmap abs arbitrary

bestBidQtyArb :: SNat n -> Gen (MarketDataAttr ( 'BestBid n "Quantity"))
bestBidQtyArb n = SBestBid n sing $=:: fmap abs arbitrary

bestAskPriceArb :: SNat n -> Gen (MarketDataAttr ( 'BestAsk n "Price"))
bestAskPriceArb n = SBestAsk n sing $=:: fmap abs arbitrary

bestAskQtyArb :: SNat n -> Gen (MarketDataAttr ( 'BestAsk n "Quantity"))
bestAskQtyArb n = SBestAsk n sing $=:: fmap abs arbitrary

genDaySimple :: Gen Day
genDaySimple =
-- "Invalid values will be clipped to the correct range, month first, then day."
  fromGregorian
    <$> QC.choose (1995, 2019) <*> QC.choose (1, 12) <*> QC.choose (1, 31)

timesTenToThe :: Num a => Word8 -> a -> a
timesTenToThe n = (* (10 ^ n))

genTodSec :: Gen Pico
genTodSec =
  let genMs   = timesTenToThe 9 <$> QC.choose (0, 999) --   leap seconds
      genSecs = timesTenToThe 12 <$> QC.choose (0, 60) -- allow
  in  fmap (MkFixed @E12) . (+) <$> genMs <*> genSecs

genTod :: Gen TimeOfDay
genTod = TimeOfDay <$> QC.choose (0, 23) <*> QC.choose (0, 59) <*> genTodSec

genLocalTimeSimple :: Gen LocalTime
genLocalTimeSimple = LocalTime <$> genDaySimple <*> genTod

{-
 - Currently, the program only works for intra-day data,
 - which demands this kind of helper
 -}
gen9To5Tod :: Gen TimeOfDay
gen9To5Tod =
  TimeOfDay <$> QC.choose (9, 16) <*> QC.choose (0, 59) <*> genTodSec

gen9To5LocalTimeSimple :: Gen LocalTime
gen9To5LocalTimeSimple = LocalTime <$> genDaySimple <*> gen9To5Tod

-- at the moment, only ilustrative
genQuoteSummary :: Rdr.ReaderT LocalTime Gen QuoteSummary
genQuoteSummary = do
  quoteAcceptTime <- localTimeOfDay <$> Rdr.ask

  Rdr.lift $
         (SIssueCode $=:: arbitrary)
    <*&> (bestBidPriceArb $ SNat @1)
    <*&> (bestBidQtyArb   $ SNat @1)
    <*&> (bestBidPriceArb $ SNat @2)
    <*&> (bestBidQtyArb   $ SNat @2)
    <*&> (bestBidPriceArb $ SNat @3)
    <*&> (bestBidQtyArb   $ SNat @3)
    <*&> (bestBidPriceArb $ SNat @4)
    <*&> (bestBidQtyArb   $ SNat @4)
    <*&> (bestBidPriceArb $ SNat @5)
    <*&> (bestBidQtyArb   $ SNat @5)
    <*&> (bestAskPriceArb $ SNat @1)
    <*&> (bestAskQtyArb   $ SNat @1)
    <*&> (bestAskPriceArb $ SNat @2)
    <*&> (bestAskQtyArb   $ SNat @2)
    <*&> (bestAskPriceArb $ SNat @3)
    <*&> (bestAskQtyArb   $ SNat @3)
    <*&> (bestAskPriceArb $ SNat @4)
    <*&> (bestAskQtyArb   $ SNat @4)
    <*&> (bestAskPriceArb $ SNat @5)
    <*&> (bestAskQtyArb   $ SNat @5)
    <*&> (SQuoteAcceptTime $=:: pure quoteAcceptTime)
    <*&> pure RNil

genPktIssueCode :: Gen (Tagged "PktIssueCode" B.ByteString)
genPktIssueCode =
  Tagged
    .   C.pack
    .   T.unpack
    .   unIsin
    .   R.unrefine
    <$> arbitrary @(Refined ValidIsin (Isin Text))

textToBS' :: Text -> B.ByteString
textToBS' = C.pack . T.unpack

pad'
  :: (Semigroup a, IsString a)
  => Tagged "left?" Bool
  -> Tagged "length" (a -> Int)
  -> Char
  -> Int
  -> a
  -> a
pad' (Tagged b) (Tagged length') c n s =
  bool flip id b (<>) (fromString $ L.replicate (n - length' s) c) s

padLT :: Char -> Int -> Text -> Text
padLT = pad' (Tagged @"left?" True) (Tagged T.length)

padRT :: Char -> Int -> Text -> Text
padRT = pad' (Tagged @"left?" False) (Tagged T.length)

padLS :: Char -> Int -> String -> String
padLS = pad' (Tagged @"left?" True) (Tagged L.length)

padRS :: Char -> Int -> String -> String
padRS = pad' (Tagged @"left?" False) (Tagged L.length)

{-
 - For more particular generators, it'd be interesting to fine-tune
 - the frequencies
 -}
genFixedDecimal
  :: Tagged "length" Int
  -> Tagged "decimalPlacesRange" (Int, Int)
  -> Gen (Tagged "Decimal" B.ByteString)
genFixedDecimal (Tagged len) (Tagged (dmin, dmax)) = do
  lfrac <- QC.choose (dmin, dmax)
  let digitGen         = QC.choose ('0', '9')
      condConsDecPoint = bool ((:) '.') id (lfrac == 0)
      lwhole           = bool (len - lfrac - 1) len (lfrac == 0)
  whole <- M.replicateM lwhole digitGen
  frac  <- M.replicateM lfrac digitGen
  return . Tagged . C.pack $ whole ++ condConsDecPoint frac

genPktPrice :: Gen (Tagged "PktPrice" B.ByteString)
genPktPrice = retag
  <$> genFixedDecimal (Tagged @"length" 5) (Tagged @"decimalPlacesRange" (0, 2))

genPktQty :: Gen (Tagged "PktQuantity" B.ByteString)
genPktQty = retag
  <$> genFixedDecimal (Tagged @"length" 7) (Tagged @"decimalPlacesRange" (0, 1))

genRandomBS' :: Int -> Gen B.ByteString
genRandomBS'  = fmap C.pack . flip M.replicateM (arbitrary @Char)

genQuoteSummaryPktHdr
  :: Rdr.ReaderT LocalTime Gen (Tagged "QuoteSummaryPktHdr" Pcap.PktHdr)
genQuoteSummaryPktHdr = do
  localTime <- Rdr.ask

  let posixSecs =
        round
          . toRational
          . utcTimeToPOSIXSeconds
          . localTimeToUTC utc
          $ localTime

  return . Tagged $ Pcap.PktHdr posixSecs 0 0 0
                                       -- ^ ^ ^ dummy

todToMilli :: TimeOfDay -> Milli
todToMilli tod =
  let h  = todHour tod * 3600
      m  = todMin tod * 60
      ms = floor . toRational . (* 1000) $ todSec tod
  in  MkFixed @E3 . fromIntegral $ (h + m) * 1000 + ms

displayPaddedTodMs' :: TimeOfDay -> String
displayPaddedTodMs' =
  padLS '0' 3
    . show @Int
    . (`mod` 1000) --
    . floor        -- a bit of hussle
    . toRational   --  due to the use of Fixed
    . (* 1000)     --
    . todSec

mkPktTod' :: TimeOfDay -> Tagged "PktTod" B.ByteString
mkPktTod' =
  (fmap Tagged . (<>))
    <$> C.pack
    .   formatTime defaultTimeLocale "%H%M%S"
    <*> C.pack
    .   L.init
    .   displayPaddedTodMs'

genQuoteSummaryPktBody
  :: Rdr.ReaderT LocalTime Gen (Tagged "QuoteSummaryPktBody" B.ByteString)
genQuoteSummaryPktBody = do
  let
    genMagicNumbers = pure $ C.pack "B6034"
    genEndMarker    = pure $ B.pack [0xff]
    best'           = L.take (5 * 2)
      $ L.cycle [unTagged <$> genPktPrice, unTagged <$> genPktQty]

  t <- unTagged . mkPktTod' . localTimeOfDay <$> Rdr.ask

  Rdr.lift
    $  fmap (Tagged . B.concat)
    .  sequenceA
    $  [genMagicNumbers, unTagged <$> genPktIssueCode, genRandomBS' 12]
    <> best'
    <> [genRandomBS' 7]
    <> best'
    <> [genRandomBS' 50, pure t, genEndMarker]

genQuoteSummaryPktBodyBlob'
  :: Tagged "QuoteSummaryPktBody" B.ByteString
  -> Gen (Tagged "QuoteSummaryPktBodyBlob" B.ByteString)
genQuoteSummaryPktBodyBlob' (Tagged pktBody) = do
  l1 <- QC.choose (0, 200)
  l2 <- QC.choose (0, 80)
  fmap (Tagged . B.concat)
    . sequenceA
    $ [genRandomBS' l1, pure pktBody, genRandomBS' l2]

genQuoteSummaryPkt' :: Gen (Tagged "QuoteSummaryPkt" Packet)
genQuoteSummaryPkt' = do
  (LocalTime <$> genDaySimple <*> genTod) >>= fmap Tagged . Rdr.runReaderT
    ((,) <$> fmap unTagged genQuoteSummaryPktHdr <*> fmap unTagged genQuoteSummaryPktBody)

genQuoteSummaryPkt :: Gen (Tagged "QuoteSummaryPkt" Packet)
genQuoteSummaryPkt = genQuoteSummaryPkt' >>= (traverse . traverse)
  (fmap unTagged . genQuoteSummaryPktBodyBlob' . Tagged)

-- leap seconds don't make sense here
addLocalTime' :: Pico -> LocalTime -> LocalTime
addLocalTime' picosecs localTime =
  let day     = localDay localTime
      tod     = localTimeOfDay localTime
      cnv     = fromIntegral
      (sc, s) = (todSec tod + picosecs) `divMod'` 60 :: (Integer, Fixed E12)
      (mc, m) = (cnv (todMin tod) + sc) `divMod` 60
      (dc, h) = (cnv (todHour tod) + mc) `divMod` 24
  in  LocalTime (addDays (cnv dc) day) $ (TimeOfDay `on` cnv) h m s

genAdjacentTimeDelta :: Rdr.ReaderT LocalTime Gen Pico
genAdjacentTimeDelta = do
  Rdr.lift $ MkFixed @E12 . timesTenToThe 9 <$> QC.choose (0, 3000)

genAdjacentQuoteSummaryPkt' :: Rdr.ReaderT LocalTime Gen (Tagged "QuoteSummaryPkt" Packet)
genAdjacentQuoteSummaryPkt' = do
  dt1 <- genAdjacentTimeDelta

  (fmap Tagged . (,))
    <$> Rdr.local (addLocalTime' dt1) (unTagged <$> genQuoteSummaryPktHdr)
    <*> Rdr.local (addLocalTime' dt1) (unTagged <$> genQuoteSummaryPktBody)

genAdjacentQuoteSummaryPkt
  :: Rdr.ReaderT LocalTime Gen (Tagged "QuoteSummaryPkt" Packet)
genAdjacentQuoteSummaryPkt =
  genAdjacentQuoteSummaryPkt' >>= Rdr.lift . (traverse . traverse)
    (fmap unTagged . genQuoteSummaryPktBodyBlob' . Tagged)

data AdjQSPktsEnv = AdjQSPktsEnv
  {
    adjQsPktsEnvGoalDeltaSecs    :: Word32
  , adjQsPktsEnvPktsDensityRange :: (Word32, Word32)
  , adjQsPktsEnvReferential      :: LocalTime
  }

-- @extra
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
{-# INLINE concatMapM #-}
concatMapM op = foldr f (return [])
 where
  f x xs = do
    x' <- op x
    if null x'
      then xs
      else do
        xs' <- xs
        return $ x' ++ xs'

genAdjacentQuoteSummaryPkts'
  :: Rdr.ReaderT AdjQSPktsEnv Gen [Tagged "QuoteSummaryPkt" Packet]
genAdjacentQuoteSummaryPkts' = do
  env <- Rdr.ask

  let referential  = adjQsPktsEnvReferential env
      deltaSecs    = adjQsPktsEnvGoalDeltaSecs env
      densityRange = adjQsPktsEnvPktsDensityRange env
      deltaList    = enumFromTo 1 deltaSecs

  density <- Rdr.lift $ QC.choose densityRange

  Rdr.lift
    $   concatMapM
          ( Rdr.runReaderT
          $ M.replicateM (fromIntegral density) genAdjacentQuoteSummaryPkt'
          )
    $   flip addLocalTime' referential
    .   MkFixed @E12
    .   timesTenToThe 12
    .   fromIntegral
    <$> deltaList

genAdjacentQuoteSummaryPkts
  :: Rdr.ReaderT AdjQSPktsEnv Gen [Tagged "QuoteSummaryPkt" Packet]
genAdjacentQuoteSummaryPkts = do
  genAdjacentQuoteSummaryPkts' >>= Rdr.lift . (traverse . traverse . traverse)
    (fmap unTagged . genQuoteSummaryPktBodyBlob' . Tagged)

genAdjPkts'
  :: Word32 -> (Word32, Word32) -> Gen [Tagged "QuoteSummaryPkt" Packet]
genAdjPkts' deltaSecs densityRange =
  AdjQSPktsEnv deltaSecs densityRange
    <$> gen9To5LocalTimeSimple
    >>= Rdr.runReaderT genAdjacentQuoteSummaryPkts

extractQuotePkt
  :: B.ByteString -> Maybe (Tagged "QuoteSummaryPktBody" B.ByteString)
extractQuotePkt =
  let p b = if B.length b == 215 && B.last b == 0xff
        then Just $ Tagged b
        else Nothing
  in  p . B.take 215 . snd . B.breakSubstring (C.pack "B6034")

quoteSumPktMagicNumbers :: PB.Parser ()
quoteSumPktMagicNumbers =
  ()
    <$ (PC.string (C.pack "B6") <?> "Data Type")
    <* (PC.string (C.pack "03") <?> "Information Type")
    <* (PC.char '4' <?> "Market Type")

quoteSumPktEndMarker :: PB.Parser ()
quoteSumPktEndMarker = () <$ PB.satisfy (== 0xff) <?> "End of Message"

quotePktIssueCode :: PB.Parser (Refined ValidIsin (Isin Text))
quotePktIssueCode =
  fmap (Isin . T.pack . C.unpack) (PB.take 12 <?> "Issue Code")
    >>= either (fail . show) return
    .   R.refine

fixedDecimal
  :: Show e
  => Tagged "length" Int
  -> Tagged "decimalPlacesRange" (Int, Int)
  -> Tagged "parser" (B.ByteString -> Either e r)
  -> PB.Parser r
fixedDecimal (Tagged len) (Tagged (dmin, dmax)) (Tagged f) = do
  raw <- PB.take len
  let mi = B.elemIndex 46 raw -- decimal point index
      r  = either (fail . show) return . f $ raw
  if B.elem 101 raw
    then fail "Exponentiation not allowed"
    else if isNothing mi && dmin > 0
      then fail "Must have a fractional part"
      else case mi of
        Nothing -> r
        Just i ->
          let lfrac = len - i - 1
          in
            if lfrac /= 0 && lfrac >= dmin && lfrac <= dmax
              then r
              else
                fail
                  $ L.concat
                      [ "Must have "
                      , show dmin
                      , " to "
                      , show dmax
                      , " decimal places"
                      ]

fixedRational'
  :: Tagged "length" Int
  -> Tagged "decimalPlacesRange" (Int, Int)
  -> PB.Parser Rational
fixedRational' taggedLen taggedDPR = fixedDecimal
  taggedLen
  taggedDPR
  (Tagged @"parser" . PB.parseOnly $ PC.rational <* PB.endOfInput)

quotePktPrice :: PB.Parser (Money.Dense CcySym)
quotePktPrice = Money.dense'
  <$> fixedRational' (Tagged @"length" 5) (Tagged @"decimalPlacesRange" (0, 2))

quotePktQty :: PB.Parser Rational
quotePktQty =
  fixedRational' (Tagged @"length" 7) (Tagged @"decimalPlacesRange" (0, 1))

mkValidTod' :: Tagged "HHMMSSuu" String -> Maybe TimeOfDay
mkValidTod' (Tagged ts) =
  let cut i = toInt . L.take 2 . L.drop i $ ts
      toInt = read @Int
      h     = cut 0
      m     = cut 2
      s     = cut 4
      ms    = cut 6
  in  guard (L.length ts == 8 && L.all PC.isDigit ts) >> makeTimeOfDayValid
        h
        m
        (MkFixed @E12 $ timesTenToThe 9 $ fromIntegral (s * 1000 + 10 * ms))

quotePktQuoteAcceptTime :: PB.Parser TimeOfDay
quotePktQuoteAcceptTime =
  (C.unpack <$> PB.take 8 <?> "Quote accept time")
    >>= maybe (fail "Invalid quote accept time") return
    .   mkValidTod'
    .   Tagged

ordinalIndicator :: Natural -> String
ordinalIndicator n =
  case n `mod` 10 of
    1 -> "st"
    2 -> "snd"
    3 -> "trd"
    _ -> "th"

bestLabel' :: KnownNat n => String -> String -> SNat n -> String
bestLabel' t q sn =
  let n = natVal sn
  in  concat ["Best ", t, " ", q, "(", show n, ordinalIndicator n, ")"]

bestBidPrice
  :: KnownNat n => SNat n -> PB.Parser (MarketDataAttr ( 'BestBid n "Price"))
bestBidPrice n =
  SBestBid n sing $=:: quotePktPrice <?> bestLabel' "bid" "price" n

bestBidQty
  :: KnownNat n => SNat n -> PB.Parser (MarketDataAttr ( 'BestBid n "Quantity"))
bestBidQty n =
  SBestBid n sing $=:: quotePktQty <?> bestLabel' "bid" "quantity" n

bestAskPrice
  :: KnownNat n => SNat n -> PB.Parser (MarketDataAttr ( 'BestAsk n "Price"))
bestAskPrice n =
  SBestAsk n sing $=:: quotePktPrice <?> bestLabel' "ask" "price" n

bestAskQty
  :: KnownNat n => SNat n -> PB.Parser (MarketDataAttr ( 'BestAsk n "Quantity"))
bestAskQty n =
  SBestAsk n sing $=:: quotePktQty <?> bestLabel' "ask" "quantity" n

quoteSummary :: PB.Parser QuoteSummary
quoteSummary =
       quoteSumPktMagicNumbers
  *>   (SIssueCode $=:: quotePktIssueCode)
  <*   PB.take 12
  <*&> (bestBidPrice $ SNat @1)
  <*&> (bestBidQty   $ SNat @1)
  <*&> (bestBidPrice $ SNat @2)
  <*&> (bestBidQty   $ SNat @2)
  <*&> (bestBidPrice $ SNat @3)
  <*&> (bestBidQty   $ SNat @3)
  <*&> (bestBidPrice $ SNat @4)
  <*&> (bestBidQty   $ SNat @4)
  <*&> (bestBidPrice $ SNat @5)
  <*&> (bestBidQty   $ SNat @5)
  <*   PB.take 7
  <*&> (bestAskPrice $ SNat @1)
  <*&> (bestAskQty   $ SNat @1)
  <*&> (bestAskPrice $ SNat @2)
  <*&> (bestAskQty   $ SNat @2)
  <*&> (bestAskPrice $ SNat @3)
  <*&> (bestAskQty   $ SNat @3)
  <*&> (bestAskPrice $ SNat @4)
  <*&> (bestAskQty   $ SNat @4)
  <*&> (bestAskPrice $ SNat @5)
  <*&> (bestAskQty   $ SNat @5)
  <*   PB.take 50
  <*&> (SQuoteAcceptTime $=:: quotePktQuoteAcceptTime)
  <*&> pure RNil
  <*   quoteSumPktEndMarker

parseQuoteSummary
  :: Tagged "QuoteSummaryPkt" Packet -> Either String QuoteSummary
parseQuoteSummary = PB.parseOnly (quoteSummary <* PB.endOfInput) . snd . unTagged

parseQuoteSummary'
  :: Tagged "QuoteSummaryPkt" Packet -> Either String QuoteSummary
parseQuoteSummary' =
  maybe (Left "Invalid Quote Summary packet") Right
    .   (traverse . traverse) (fmap unTagged . extractQuotePkt)
    >=> parseQuoteSummary

doubleHardline :: Doc ann
doubleHardline = PP.hardline <> PP.hardline

printQuoteSummary :: QuoteSummary -> IO ()
printQuoteSummary =
  PP.renderIO System.IO.stdout
    . PP.layoutPretty  PP.defaultLayoutOptions
    . pretty

instance Pretty (MarketDataAttr 'QuoteAcceptTime) where
  pretty (MarketDataAttr tod) =
    let hms = T.pack . formatTime defaultTimeLocale "%H:%M:%S" $ tod
    in  pretty $ hms <> "." <> (T.pack . displayPaddedTodMs' $ tod)

displayPrice' :: Money.Dense CcySym -> Text
displayPrice' = CNV.denseToDecimal CNV.defaultDecimalConf CNV.Round

prettyPrice' :: Money.Dense CcySym -> Doc ann
prettyPrice' = pretty . displayPrice'

bestQtyDecimalConf' :: CNV.DecimalConf
bestQtyDecimalConf' = CNV.DecimalConf
  { CNV.decimalConf_separators  = CNV.separatorsDot
  , CNV.decimalConf_leadingPlus = False
  , CNV.decimalConf_digits      = 1
  , CNV.decimalConf_scale       = maybe (error "Impossible scale") id
                                    $ CNV.scaleFromRational 1
  }

displayQty' :: Rational -> Text
displayQty' = CNV.rationalToDecimal bestQtyDecimalConf' CNV.Round

prettyQty' :: Rational -> Doc ann
prettyQty' = pretty . displayQty'

instance Pretty (MarketDataAttr 'IssueCode) where
  pretty = pretty . unIsin . R.unrefine . unMarketDataAttr

instance Pretty (MarketDataAttr ('BestBid n "Price")) where
  pretty = prettyPrice' . unMarketDataAttr

instance Pretty (MarketDataAttr ('BestBid n "Quantity")) where
  pretty = prettyQty' . unMarketDataAttr

instance Pretty (MarketDataAttr ('BestAsk n "Price")) where
  pretty = prettyPrice' . unMarketDataAttr

instance Pretty (MarketDataAttr ('BestAsk n "Quantity")) where
  pretty = prettyQty' . unMarketDataAttr

instance Pretty QuoteSummary where
  pretty q =
    let acceptTime = q ^. rlens @'QuoteAcceptTime
        issueCode  = q ^. rlens @'IssueCode
        bqty1      = q ^. rlens @('BestBid 1 "Quantity")
        bprice1    = q ^. rlens @('BestBid 1 "Price")
        bqty2      = q ^. rlens @('BestBid 2 "Quantity")
        bprice2    = q ^. rlens @('BestBid 2 "Price")
        bqty3      = q ^. rlens @('BestBid 3 "Quantity")
        bprice3    = q ^. rlens @('BestBid 3 "Price")
        bqty4      = q ^. rlens @('BestBid 4 "Quantity")
        bprice4    = q ^. rlens @('BestBid 4 "Price")
        bqty5      = q ^. rlens @('BestBid 5 "Quantity")
        bprice5    = q ^. rlens @('BestBid 5 "Price")
        aqty1      = q ^. rlens @('BestBid 1 "Quantity")
        aprice1    = q ^. rlens @('BestBid 1 "Price")
        aqty2      = q ^. rlens @('BestBid 2 "Quantity")
        aprice2    = q ^. rlens @('BestBid 2 "Price")
        aqty3      = q ^. rlens @('BestBid 3 "Quantity")
        aprice3    = q ^. rlens @('BestBid 3 "Price")
        aqty4      = q ^. rlens @('BestBid 4 "Quantity")
        aprice4    = q ^. rlens @('BestBid 4 "Price")
        aqty5      = q ^. rlens @('BestBid 5 "Quantity")
        aprice5    = q ^. rlens @('BestBid 5 "Price")
        p          = pretty
        -- prettyprinter has no proper support for multi-column layouts,
        -- so lets just right pad the output
        p' qty price =
          PP.width (p'' qty price) (\w -> pretty $ T.replicate (13 - w) " ")
        p'' qty price = p qty <> "@" <> p price

    in  PP.fillSep
          [ p acceptTime
          , p issueCode
          , PP.hardline
          , p' bqty5 bprice5
          , p' bqty4 bprice4
          , p' bqty3 bprice3
          , p' bqty2 bprice2
          , p'' bqty1 bprice1
          , PP.hardline
          , p' aqty5 aprice5
          , p' aqty4 aprice4
          , p' aqty3 aprice3
          , p' aqty2 aprice2
          , p'' aqty1 aprice1
          ] <> doubleHardline

newtype Bindings p v = Bindings
  { unBindings :: [(p, v)]
  }

{-# SPECIALISE sortIntervalByQuoteAcceptTime
  :: Monad m
  => ConduitT
       QuoteSummary
       QuoteSummary
       ( S.StateT
           ( Tagged "TodReferentialAsMilli" Milli
           , Bindings (Tagged "TodAsMilli" Milli) QuoteSummary
           )
           m
       )
       () #-}

takeWhile'
  :: Monad m
  => (a -> Bool)
  -> ConduitT a o m (Tagged "Leftover" (Maybe a), [a])
takeWhile' f =
  let loop g = do
        mx <- await
        case mx of
          Nothing -> return $ g []
          Just x  -> if f x
            then loop $ g . (x :)
            else return . bimap (Just x <$) id $ g []
  in  loop $ (,) (Tagged Nothing)
{-# INLINE takeWhile' #-}

sortIntervalByQuoteAcceptTime
  :: ( q ~ Rec MarketDataAttr attrs
     , RElem 'QuoteAcceptTime attrs (RIndex 'QuoteAcceptTime attrs)
     , Monad m
     )
  => ConduitT
       q
       q
       ( S.StateT
           ( Tagged "TodReferentialAsMilli" Milli
           , Bindings (Tagged "TodAsMilli" Milli) q
           )
           m
       )
       ()
sortIntervalByQuoteAcceptTime = do
  (todReferential, residual) <- bimap id unBindings <$> S.lift S.get

  let toMs             = todToMilli . unAttrRecQuoteAcceptTime'
      toBinding        = (,) <$> Tagged @"TodAsMilli" . toMs <*> id
      condConsLeftover = maybe id (:) . unTagged
      t0 :: Tagged "TodAsMilli" Milli
      t0 = retag todReferential

      t1 = t0 + 3
      t2 = t0 + 6

  (mleftover, chunk) <- CC.map toBinding .| takeWhile' ((<= t2) . fst)

  let (intermediate, newResidual) = L.partition
        ((<= t1) . fst)
        (condConsLeftover mleftover residual ++ chunk)

      sorted            = sortOn fst $ intermediate

      newTodReferential = retag t1

  S.lift $ S.put (newTodReferential, Bindings newResidual)
  CList.sourceList . fmap snd $ sorted

{-# SPECIALISE sortByQuoteAcceptTime
  :: Monad m => ConduitT QuoteSummary QuoteSummary m () #-}

sortByQuoteAcceptTime
  :: ( q ~ Rec MarketDataAttr attrs
     , RElem 'QuoteAcceptTime attrs (RIndex 'QuoteAcceptTime attrs)
     , Monad m
     )
  => ConduitT q q m ()
sortByQuoteAcceptTime = CC.peek >>= M.mapM_ f
 where
  f q = do
    let toMs             = todToMilli . unAttrRecQuoteAcceptTime'
        toBinding        = (,) <$> Tagged @"TodAsMilli" . toMs <*> id
        peekedTodSecAsMs = Tagged @"TodAsMilli" . toMs $ q
        condConsLeftover = maybe id (:) . unTagged

    (mleftover, firstChunk) <- CC.map toBinding
      .| takeWhile' ((<= peekedTodSecAsMs + 3) . fst)

    let todReferential =
          Tagged @"TodReferentialAsMilli"
            . toMs
            . snd
            . L.minimumBy (compare `on` fst)
            $ firstChunk

        loop = do
          mx <- await
          case mx of
            Nothing -> return ()
            Just x  -> do
              S.lift $ S.get >>= S.put . fmap
                (Bindings . (toBinding x :) . unBindings)
              sortIntervalByQuoteAcceptTime
              loop

        handleResidual = let sc = sortIntervalByQuoteAcceptTime in sc >> sc

    CL.evalStateC
        (todReferential, Bindings $ condConsLeftover mleftover firstChunk)
      $  loop
      >> handleResidual

data Options = Options
  { optSortByQuoteAcceptTime :: Bool
  , optFile                  :: Maybe FilePath
  }

parseOptions :: O.Parser Options
parseOptions =
  Options
    <$>    O.switch (O.short 'r' <> O.help "Sort by quote accept time")
    <*>    fmap Just (O.argument O.str (O.metavar "FILE"))
    O.<**> O.helper

programOptions :: O.ParserInfo Options
programOptions = O.info parseOptions (O.fullDesc <> O.failureCode 64)

defaultMain :: (Options -> IO ()) -> IO ()
defaultMain main = O.execParser programOptions >>= main
