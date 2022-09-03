module MainWithConduit where

import           CodeSample

import           Data.Bool                (bool)
import           Data.Conduit
import qualified Data.Conduit.Combinators as CC
import           Data.Tagged
import           Network.Pcap.Conduit     (Packet)

-- this is not as beautiful as it could be :^)
-- but it's something
data MainF m r = MainF
  { mainfSource :: Maybe FilePath -> ConduitT () Packet m ()
  , mainfClosureConduit :: ConduitT QuoteSummary Void m r
  , mainfOptions :: Options
  }

interpret :: Monad m => MainF m r -> m r
interpret mainF = do
  let
    source             = mainfSource mainF
    closure            = mainfClosureConduit mainF
    options            = mainfOptions mainF

    (fileOpt, sortOpt) = (,) <$> optFile <*> optSortByQuoteAcceptTime $ options

    infixl 3 `ifSortOpt`
    ifSortOpt = bool const (.|) sortOpt

  runConduit
    $           (source fileOpt)
    .| CC.concatMap (fmap Tagged . traverse (fmap unTagged . extractQuotePkt))
    .|          CC.concatMap parseQuoteSummary
    `ifSortOpt` sortByQuoteAcceptTime
    .|          closure
