{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Client where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Network.Wreq.Session (withAPISession, Session (..), withSession)
import Data.Aeson.Lens (key,_String, _Array, _Number, _Integral, _Integer)
import Data.Aeson (Value(..))
import Control.Lens hiding (Level (..))

import Data.Text (Text (..), pack)
import Data.Monoid
import Data.String.Conversions
import Data.Time

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State as ST
import Control.Monad.Trans
import Control.Applicative (ZipList (..))

import Types

readSess = fmap fst ask

readClientConfig :: Rdr ClientConfig
readClientConfig = fmap snd ask

readBaseURL = baseURLWithHost <$> readClientConfig

readUser :: Rdr String
readUser = fmap (fst . hostCredentials . snd) ask

readPass :: Rdr String
readPass = fmap (snd . hostCredentials . snd) ask

------------------------ API QUERYING FUNCTIONS --------------------

authenticate :: Session -> IO APIKey
authenticate sess = do 
        auth_r <- S.post sess login_url credentials
        let api_key = (auth_r ^. responseBody . key "user" . key "key" . _String)
        return api_key

loginURL = "login/auth.php"
authenticate' :: Rdr APIKey
authenticate' = do 
        sess <- readSess
        baseURL <- readBaseURL
        usr <- readUser
        pass <- readPass
        let loginCredentials = ["username" := usr, "password" := pass]
        auth_r <- liftIO $ S.post sess (baseURL <> loginURL) loginCredentials
        let api_key = (auth_r ^. responseBody . key "user" . key "key" . _String)
        return api_key

waitingTicketsForServiceId :: Session -> Int -> IO Int
waitingTicketsForServiceId sess n = do 
        r <- S.get sess $ waiting_list_url n
        let tickets =  (r ^. responseBody . key "tickets" . _Array)
        let num_tickets = length tickets
        return (num_tickets)

waitingTicketsForService :: TurnstatService -> Rdr Int
waitingTicketsForService n = do 
        sess <- readSess
        baseURL <- readBaseURL
        r <- liftIO $ S.get sess $ (baseURL <> waitingListURL (serviceID n)) 
        let tickets =  (r ^. responseBody . key "tickets" . _Array)
        let num_tickets = length tickets
        return (num_tickets)

-- | Returns a list of ints specifying then number of waiting tickets by service
allWaitingTickets :: Session -> IO [Int]
allWaitingTickets sess = mapM (waitingTicketsForServiceId sess) [1..3]

allWaitingTickets' :: Rdr [(TurnstatService, Int)]
allWaitingTickets' = do
    apiKey <- authenticate'
    services <- getAllServices' apiKey
    let ids = map (fromIntegral . serviceID) services
    let serviceNames = map serviceName services
    waiting <- mapM waitingTicketsForService services
    return $ zip services waiting

-- | Returns a list a of all services
-- withSession $ \s -> authenticate s >>= getAllServices s 
getAllServicesURL = "setup/services_get_all.php"
getAllServices :: Session -> APIKey -> IO [TurnstatService]
getAllServices sess api_key = do 
        r <- S.getWith (post_headers api_key) sess get_all_services_url
        let names =  toListOf (responseBody . key "result" .  _Array . traverse . key "name" . _String) r
        let servId =  toListOf (responseBody . key "result" .  _Array . traverse . key "id" . _String) r
        let servChar =  toListOf (responseBody . key "result" .  _Array . traverse . key "char" . _String) r
        let enabled =  toListOf (responseBody . key "result" .  _Array . traverse . key "enabled" . _String) r
        let readInt = read . cs :: Text -> Integer 
        let adapt = ZipList . map cs
        let res = getZipList (TurnstatService <$> (readInt <$> ZipList servId) <*> (adapt names) <*> (adapt servChar) <*> (adapt enabled))
        return (res)

-- | Returns a list of all posible services
getAllServices' :: APIKey -> Rdr [TurnstatService]
getAllServices' api_key = do 
        sess <- readSess
        baseURL <- readBaseURL
        r <- liftIO $ S.getWith (post_headers api_key) sess $ baseURL <> getAllServicesURL
        let names =  toListOf (responseBody . key "result" .  _Array . traverse . key "name" . _String) r
        let servId =  toListOf (responseBody . key "result" .  _Array . traverse . key "id" . _String) r
        let servChar =  toListOf (responseBody . key "result" .  _Array . traverse . key "char" . _String) r
        let enabled =  toListOf (responseBody . key "result" .  _Array . traverse . key "enabled" . _String) r
        let readInt = read . cs :: Text -> Integer 
        let adapt = ZipList . map cs
        let res = getZipList (TurnstatService <$> (readInt <$> ZipList servId) <*> (adapt names) <*> (adapt servChar) <*> (adapt enabled))
        return (res)

-- | Returns a list of all posible services
getAllSlotsURL = "setup/slots_get_all.php"
getAllSlots' :: APIKey -> Rdr [TurnstatSlot]
getAllSlots' api_key = do 
        sess <- readSess
        baseURL <- readBaseURL
        r <- liftIO $ S.getWith (post_headers api_key) sess $ baseURL <> getAllSlotsURL
        let names =  toListOf (responseBody . key "result" .  _Array . traverse . key "name" . _String) r
        let slotId =  toListOf (responseBody . key "result" .  _Array . traverse . key "id" . _String) r
        let enabled =  toListOf (responseBody . key "result" .  _Array . traverse . key "enabled" . _String) r
        let adapt f = ZipList . map f
        --let res = getZipList (TurnstatSlot <$> (adapt readAny slotId) <*> (adapt readAny names) <*> (adapt readAny enabled))
        let res = getZipList (TurnstatSlot <$> (adapt cs slotId) <*> (adapt cs names) <*> (adapt cs enabled))
        return (res)

readAny :: Read a => Text -> a 
readAny =  read . cs

createTicket :: Session -> APIKey -> ServiceID -> Origin -> IO ()
createTicket sess api_key sId origin = do
        let params = ["service" := sId, "origin" := show origin]
        res <- S.postWith (post_headers api_key) sess create_ticket_url params
        print res
        let printable =  res ^. responseBody . key "printable" . _String
        print $ "created ticket " ++ (show printable)
        return ()

-- | Creates a ticket for some origin and service id.
createTicketURL =  "ticket_management/create.php"
createTicket' :: APIKey -> ServiceID -> Origin -> Rdr TurnstatTicket
createTicket' api_key sId origin = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = ["service" := sId, "origin" := show origin]
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> createTicketURL) params
        --liftIO $ print res
        let printable =  res ^. responseBody . key "printable" . _String
        let tuid =  res ^. responseBody . key "tuid" . _String
        let tkt = TurnstatTicket (cs tuid) (read $ cs printable) 
        return tkt 

-- | Changes a tickets status from SERVING to FINISHED
finishTicketURL = "ticket_management/finish.php"
finishTicket :: APIKey -> Int -> Rdr String
finishTicket api_key tid = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = ["ticket_id" := show tid]
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> finishTicketURL) params
        liftIO $ print res
        let didSucced =  res ^. responseBody . key "result" . _String
        return $ cs didSucced

-- | Calls a specific ticket changing its status to SERVING
callArbitraryTicketURL = "ticket_management/call_arbitrary.php"
callArbitraryTicket :: APIKey -> Int -> Rdr String
callArbitraryTicket api_key tid = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = ["ticket_id" := show tid]
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> callArbitraryTicketURL) params
        liftIO $ print res
        let didSucced =  res ^. responseBody . key "result" . _String
        return $ cs didSucced


-- | Starts calling tickets. Needs to be used in conjuction with withModule
callTicketsURL = "ticket_management/call.php"
callTickets :: APIKey -> Rdr (Maybe Integer)
callTickets api_key = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = [] :: [FormParam] 
        --res <- liftIO $ S.getWith (post_headers api_key) sess (baseURL <> callTicketsURL)
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> callTicketsURL) params
        liftIO $ print res
        let ticketID =  res ^? responseBody . key "ticket_id" . _Integer
        return ticketID

getWaitingTicketsURL = "ticket_management/get_waiting_tickets.php"
getWaitingTickets :: APIKey -> Rdr String
getWaitingTickets api_key = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = [] :: [FormParam] 
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> getWaitingTicketsURL) params
        liftIO $ print res
        let ticketID =  res ^. responseBody . key "count_tickets" . key "count" . _String
        return (cs ticketID)


-- | Sets the role for the current logged user
setRoleURL = "placement/setrole"
setRole :: Role -> APIKey -> Rdr ()
setRole slotID api_key = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = ["role" := show "USER"]
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> setRoleURL) params
        liftIO $ print res
        let slotName =  res ^. responseBody . key "role" . _String
        return ()

-- | Assigns a slot with id n and return the name of the slot.
setSlotURL = "placement/place"
setSlot :: Int -> APIKey -> Rdr String
setSlot slotID api_key = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = ["slot_id" := show slotID]
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> setSlotURL) params
        liftIO $ print res
        let slotName =  res ^. responseBody . key "slot_name" . _String
        return (cs slotName)

printTicketURL = "/ticket_management/print.php"
-- | printTicket t p s prints a ticket with id t on printer p 
printTicket :: TicketID -> PrinterID -> APIKey -> Rdr String
printTicket ticket_id printer_id api_key = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = ["ticket_id" := show ticket_id, "printer_id" := printer_id]
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> printTicketURL) params
        --liftIO $ print res
        let resultString =  res ^. responseBody . key "result" . _String
        return (cs resultString)

post_headers :: APIKey -> Options
post_headers api_key = defaults & header "turnstat-api-key" .~ [cs api_key]
              -- & header "Content-Type" .~ [cs ("application/x-www-form-urlencoded"::String)]
              -- & auth ?~ basicAuth "usuario" "12345678"

------------------------ UTILITIES -------------------
sleep n = threadDelay $ n * 10 ^ 6 

-- authenticates for the first time: 

{-
initialState :: Session -> IO ControlState
initialState sess = do
    c <- liftIO getCurrentTime
    initial_key <- authenticate sess
    return $ ControlState (cs initial_key) c

elapsedMinutes :: TimeState Int
elapsedMinutes = do
    r <- liftIO getCurrentTime
    p <- currentTime <$> ST.get
    return $ round $ diffUTCTime r p

putAPIKey :: String -> TimeState ()
putAPIKey key = do
    c <- liftIO $ getCurrentTime
    ST.put $ ControlState key c

reauthenticate sess = do
    et <- elapsedMinutes
    case et > 60 of
        True -> do
            liftIO $ putStrLn "Reauthenticating"
            apiK <- liftIO $ authenticate sess
            putAPIKey $ cs apiK
            return apiK
        False -> do
            (cs . apiKey) <$> ST.get

authentication = \s -> initialState s >>= (evalStateT . reauthenticate) s
-}

---------------------- CONSTANTS ----------------------
baseURLWithHost cc =  "http://" ++ (hostIP cc) ++ "/index.php/"
waitingListURL n = "ws/list_waiting_tickets_service_complete/" <> (show n)

base_url, login_url, create_ticket_url :: String 
base_url = "http://172.16.0.4/index.php/"
--base_url = "http://10.1.2.26/index.php/"
login_url = base_url <> "login/auth.php"

log_file = "logging.txt"

waiting_list_url:: Int -> String 
waiting_list_url n = base_url <> "ws/list_waiting_tickets_service_complete/" <> (show n)
create_ticket_url = base_url <> "ticket_management/create.php"
get_all_services_url = base_url <> "setup/services_get_all.php"

(user,pass) = ("usuario", "12345678") :: (String,String)

credentials :: [FormParam]
credentials = ["username" := user, "password" := pass]


