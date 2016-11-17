import Test.Hspec
import Client
import Types
import Process
import Network.Wreq.Session (withSession, Session(..))
import Control.Monad.Trans

import GenericUtils

main :: IO ()
main = hspec spec


isEqual f x = f `is` (== x)


spec :: Spec
spec = do
    describe "Running Networking tests" $ do
        describe "Retreiving data from default host" $ do
            it "There are enabled services" $ do
                withOptions (ClientOptions defaultClientConfig (ShowInfo ServicesInfo)) $ do
                    api_key <- authenticate'
                    services <- getAllServices' api_key
                    let enabledServices  = filter serviceEnabled services
                    liftIO (enabledServices `shouldNotSatisfy` null)

        describe "Authentication" $ do
            it "Authentication always returns a non empty API Key" $ do
                withOptions (ClientOptions defaultClientConfig (ShowInfo ServicesInfo)) $ do
                    api_key <- authenticate'
                    liftIO (show api_key `shouldNotSatisfy` null)

        describe "Server functionality" $ do
            it "Requesting two tiquets simultaniously never results in same Printable" $ do
                (t1,t2) <- requestSameTicket' defaultClientConfig 1 GUIDED BUTTON
                let (p1,p2) = (printable t1, printable t2)
                (p1 `shouldSatisfy` \p -> printableNumber p == (printableNumber p2 + 1) || printableNumber p2 == (printableNumber p + 1))

            it "Can always sit on a random available slot" $ do
                withOptions (ClientOptions defaultClientConfig (ShowInfo ServicesInfo)) $ do
                    api_key <- authenticate'
                    allSlotsIDs <- (map slotID . filter slotEnabled) <$> getAllSlots api_key
                    someSlotID <- liftIO $ chooseRIO allSlotsIDs
                    slotName <- setSlot someSlotID api_key
                    liftIO $ putStrLn $ "Using slot named " ++ slotName
                    liftIO (slotName `shouldNotSatisfy` null)

