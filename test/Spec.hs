import Test.Hspec
import Client
import Types
import Lib
import Network.Wreq.Session (withSession, Session(..))
import Control.Monad.Trans

main :: IO ()
main = hspec spec


spec :: Spec
spec = do
    describe "Retreiving data" $ do
        it "There are enabled services" $ do
            withOptions (ClientOptions defaultClientConfig ShowServicesInfo) $ do
                api_key <- authenticate'
                services <- getAllServices' api_key
                let enabledServices  = filter ((== "t") . serviceEnabled) services
                liftIO (enabledServices `shouldNotSatisfy` null)

    describe "Authentication" $ do
        it "Authentication always returns a non empty API Key" $ do
            withOptions (ClientOptions defaultClientConfig ShowServicesInfo) $ do
                api_key <- authenticate'
                liftIO (show api_key `shouldNotSatisfy` null)

    describe "Server functionality" $ do
        it "Requesting to tiquest simultaniously never results in same Printable" $ do
            (p1,p2) <- requestSameTicket' defaultClientConfig 1 GUIDED BUTTON
            (p1 `shouldSatisfy` \p -> printableNumber p == (printableNumber p2 + 1) || printableNumber p2 == (printableNumber p + 1))
