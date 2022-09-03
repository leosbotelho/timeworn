{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           CodeSample                 hiding (defaultMain)
import qualified MainWithConduit

import           Control.DeepSeq            (NFData (..))
import           Criterion.Main
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.Bool                  (bool)
import qualified Data.Conduit.Combinators   as CC
import qualified Data.Conduit.List          as CList
import           Data.Functor.Identity      (Identity)
import           Data.Maybe                 (isJust, maybe)
import           Data.Tagged
import qualified Network.Pcap               as Pcap
import           Network.Pcap.Conduit       (Packet)
import           Network.Pcap.Conduit       (sourceOffline)
import qualified Options.Applicative        as O
import qualified System.Environment         as SysEnv
import           System.FilePath.Posix      (takeBaseName)
import qualified Test.QuickCheck            as QC

instance NFData Pcap.PktHdr where
  rnf pkt = pkt `seq` ()

pureMain :: Tagged "sort?" Bool -> [Packet] -> Identity ()
pureMain (Tagged shouldSort) l =
  MainWithConduit.interpret $ MainWithConduit.MainF
    (const $ CList.sourceList l)
    (CC.peekForever $ CC.drop 1)
    (Options shouldSort Nothing)

ioMain :: Tagged "sort?" Bool -> [Packet] -> IO ()
ioMain (Tagged shouldSort) l =
  MainWithConduit.interpret $ MainWithConduit.MainF
    (const $ CList.sourceList l)
    (CC.mapM_ printQuoteSummary)
    (Options shouldSort Nothing)


ioMain' :: Tagged "sort?" Bool -> Tagged "sourceFile" FilePath -> IO ()
ioMain' (Tagged shouldSort) mpath =
  MainWithConduit.interpret $ MainWithConduit.MainF
    (maybe (error "Impossible source file path") sourceOffline)
    (CC.mapM_ printQuoteSummary)
    (Options shouldSort (Just $ unTagged mpath))

benchMains
  :: Tagged "benchIO?" Bool -> [Tagged "QuoteSummaryPkt" Packet] -> [Benchmark]
benchMains (Tagged b) taggedPkts =
  let pkts = fmap unTagged taggedPkts
  in  [ bgroup
          "main pure"
          [ bench "without sort" $ whnf (pureMain $ Tagged False) pkts
          , bench "with sort" $ whnf (pureMain $ Tagged True) pkts
          ]
      ]
        ++ flip
             (bool [])
             b
             [ bgroup
                 "main IO"
                 [ bench "without sort" $ whnfAppIO (ioMain $ Tagged False) pkts
                 , bench "with sort" $ whnfAppIO (ioMain $ Tagged True) pkts
                 ]
             ]

data BenchOptions = BenchOptions
  { benchOptFile          :: Maybe FilePath
  , benchOptBenchIO       :: Bool
  , benchOptCriterionArgs :: [String]
  }

parseBenchOptions :: O.Parser BenchOptions
parseBenchOptions =
  BenchOptions
    <$> O.optional
          (O.strOption $ O.long "bench-source-file" <> O.help
            "Include benchmarks with the file as the source"
          )
    <*> O.switch (O.long "bench-io-main" <> O.help "Include IO main benchmarks")
    <*> O.many (O.strArgument (O.metavar "CRITERION_ARGS..."))

benchProgramOptions :: O.ParserInfo BenchOptions
benchProgramOptions =
  (O.info parseBenchOptions (O.fullDesc <> O.failureCode 64 <> O.forwardOptions)
  )

main :: IO ()
main = do
  options <- O.execParser benchProgramOptions
  let benchMains' = benchMains . Tagged @"benchIO?" . benchOptBenchIO $ options
      mpath       = benchOptFile options
      path        = maybe (error "Impossible source file path") id mpath

  SysEnv.withArgs (benchOptCriterionArgs options)
    $  defaultMain
    $  [ bgroup
         "parse"
         [ bgroup
             "quote packet"
             [ bgroup
                 "summary"
                 [ env
                   (QC.generate genPktIssueCode)
                   ( bench "Issue Code"
                   . whnf (parseOnly quotePktIssueCode . unTagged)
                   )
                 , env
                   (QC.generate genPktPrice)
                   (bench "Price" . whnf (parseOnly quotePktPrice . unTagged))
                 , env
                   (QC.generate genPktQty)
                   (bench "Quantity" . whnf (parseOnly quotePktQty . unTagged))
                 , env
                   (QC.generate $ mkPktTod' <$> genTod)
                   ( bench "Quote Accept Time"
                   . whnf (parseOnly quotePktQuoteAcceptTime . unTagged)
                   )
                 , env (snd . unTagged <$> QC.generate genQuoteSummaryPkt)
                       (bench "Extract quote pkt" . whnf extractQuotePkt)
                 , bgroup
                   "Packet"
                   [ env (QC.generate genQuoteSummaryPkt')
                         (bench "" . whnf parseQuoteSummary)
                   ]
                 ]
             ]
         ]
       , env (QC.generate $ genAdjPkts' 30 (400, 600))
         $ (bgroup "30s 400-600 pkts per sec" . benchMains')
       , env (QC.generate $ genAdjPkts' 30 (1500, 3000))
         $ (bgroup "30s 1500-3000 pkts per sec" . benchMains')
       , env (QC.generate $ genAdjPkts' 300 (400, 600))
         $ (bgroup "5min 400-600 pkts per sec" . benchMains')
       , env (QC.generate $ genAdjPkts' 300 (1500, 3000))
         $ (bgroup "5min 1500-3000 pkts per sec" . benchMains')
       ]
    ++ flip
         (bool [])
         (isJust mpath)
         [ bgroup
             "file"
             [ bgroup
                 (takeBaseName path)
                 [ bench "without sort"
                   $ whnfAppIO (ioMain' (Tagged False)) (Tagged path)
                 , bench "with sort"
                   $ whnfAppIO (ioMain' (Tagged True)) (Tagged path)
                 ]
             ]
         ]
