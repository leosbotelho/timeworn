import qualified CodeSample               (defaultMain, printQuoteSummary)

import qualified Data.Conduit.Combinators as CC
import           MainWithConduit          (MainF (..))
import qualified MainWithConduit
import           Network.Pcap.Conduit

main :: IO ()
main = do
  let deconstructFileOpt = maybe (error "Impossible source file path") id
      mainF              = MainF (sourceOffline . deconstructFileOpt)
                                 (CC.mapM_ CodeSample.printQuoteSummary)

  CodeSample.defaultMain $ MainWithConduit.interpret . mainF
