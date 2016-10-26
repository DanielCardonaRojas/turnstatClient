{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- Maybe? {-# LANGUAGE OverloadedLists #-}

module Lib where

import Network.Wreq
import qualified Network.Wreq.Session as S
import Network.Wreq.Session (withAPISession, Session (..), withSession)
import Data.Aeson.Lens (key,_String, _Array, _Number, _Integral, _Integer)
import Data.Aeson (Value(..))
import Control.Lens hiding (Level (..))

import Data.Text (Text (..), pack)
import Data.Monoid
import System.Random
import Data.String.Conversions
import Data.Maybe (fromJust)
import Data.Time

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State as ST
import Control.Monad.Trans
import Control.Applicative

--import Control.Monad.Log
--import Control.Monad.Log.Label


import Options.Applicative hiding (header, info)
import qualified Options.Applicative as OP
import Data.Monoid
import Control.Monad

----------------------- QUERY DYD TURNSTAT v.3.1 ------------------ 
{-
A client to query turnstat  server

a basic usage of querying functions can be made using the with session to handle cookies AUTOMATICALLY.
Functions that need an api_key in the header request can be used like this e.g:

    withSession $ \s -> authenticate s >>= getAllServices s
    or
    withSession $ \s -> authenticate s >>= (\k -> runReaderT (callTickets k) (s, defaultClientConfig))

COMPILATION: 
    Compile with stack: stack ghc -- turnstatClient -o turnstatClient

TODO: Avoid having to request an API key every time.
-}

---------------------- TYPES -----------------------
type APIKey = Text
type ServiceID = Int
type SlotID = Int
type TicketID = Int
data Origin = GUIDED | BUTTON | USER | FILAPP deriving (Show, Eq, Read, Enum, Bounded)
data Role = RUSER | AUDIT deriving (Show, Eq, Read, Enum, Bounded)

data TurnstatService  = TurnstatService
        {serviceID :: Integer
        , serviceName :: String
        , serviceLetter :: String
        , serviceEnabled :: String
        } deriving (Show, Eq)

data TurnstatTicket = TurnstatTicket
    { tuid :: String
    , printable :: String
    } deriving (Show, Eq)


data ClientConfig = ClientConfig
  { 
    hostIP  :: String, -- ^ Used for base url
    hostCredentials :: (String, String) -- ^ User, Pass
  } deriving (Show,Eq)

defaultClientConfig = ClientConfig "172.16.0.4" ("usuario","12345678")

data Command 
    = CreateTicket Origin ServiceID
    | CreateDuplicate Origin Origin Int
    | CreateRandomTicket Int
    | ShowServicesInfo -- ^ Can be later generalized to show other types of information
    | Periodic Int  -- ^ Creates tickets forever not exceeding some count, 
    | CallArbitrary Int  -- ^ Calls an arbitrary ticket given its id
    deriving (Show, Eq)

data ClientOptions = ClientOptions 
    {
      clientConfig :: ClientConfig
    , clientCommand :: Command
    } deriving (Show, Eq)

data ControlState = ControlState {apiKey :: String, currentTime :: UTCTime} deriving (Show,Eq)
type TimeState a = StateT ControlState IO a


-- | The data that most querying functions will use is (the session and a Clientconfig)
type Rdr a = ReaderT (Session, ClientConfig) IO a

readSess = fmap fst ask

readClientConfig :: Rdr ClientConfig
readClientConfig = fmap snd ask

readBaseURL = baseURLWithHost <$> readClientConfig

readUser :: Rdr String
readUser = fmap (fst . hostCredentials . snd) ask

readPass :: Rdr String
readPass = fmap (snd . hostCredentials . snd) ask

--------- COMMAND LINE OPTION PARSER ---------
opts :: ParserInfo ClientOptions
opts = 
    OP.info (helper <*> allCommands)
              ( fullDesc
             <> progDesc "Query DyD TurnStat v3.1.1 API"
             <> OP.header "\n\n************************* TURNSTAT CLIENT v0.0.1 ***********************\n\n" )
     where
         allCommands = 
            ClientOptions 
                <$> clientConfigParser
                <*> commandParser

clientConfigParser :: Parser ClientConfig
clientConfigParser = ClientConfig
    <$> strOption
        (  long "host"
        <> metavar "TARGET"
        <> help "Host running TurnStat")
    <*> credentials
    where 
        credentials = 
            (,) <$> strOption 
                        (long "user" <> short 'u' <> metavar "USERNAME" 
                        <> help "User used to connect to turnstat" 
                        <> showDefault <> value "usuario")
                <*> strOption 
                        (long "pass" <> metavar "PASSWORD" 
                        <> help "Users pass used to connect to turnstat" <> value "12345678")


commandParser :: Parser Command
commandParser = 
    subparser (command "create" $ OP.info (helper <*> createTicketCommand) 
                                          (fullDesc <> progDesc "Request a new ticket"))
    <|> subparser (command "dcreate" $ OP.info (helper <*> createDuplicateCommand) 
                                          (fullDesc <> progDesc "Create two tickets at the same time"))
    <|> subparser (command "rcreate" $ OP.info (helper <*> createRandomCommand) 
                                          (fullDesc <> progDesc "Create one or more random tickets"))
    <|> subparser (command "services" $ OP.info (helper <*> showServicesCommand) 
                                          (fullDesc <> progDesc "Get all available services"))
    <|> subparser (command "periodic" $ OP.info (helper <*> createPeriodicallyCommand) 
                                          (fullDesc <> progDesc "Create tickets forever not exceeding some limit"))
    <|> subparser (command "callticket" $ OP.info (helper <*> callArbitraryTicketCommand) 
                                          (fullDesc <> progDesc "Call any ticket by id"))
    where 
        createTicketCommand = CreateTicket 
            <$> option auto (long "origin" <> metavar "ORIGIN" <> help "Origin for ticket")
            <*> option auto (long "service" <> metavar "SERVICE" <> help "Service ID")
        createDuplicateCommand = CreateDuplicate 
            <$> option auto (long "origin" <> metavar "ORIGIN" <> help "Origin for first ticket")
            <*> option auto (long "origin2" <> metavar "ORIGIN" <> help "Origin for second ticket")
            <*> option auto (long "service" <> metavar "SERVICE" <> help "Serive ID")
        createRandomCommand = CreateRandomTicket
            <$> option auto (long "count" <> metavar "COUNT" <> help "How many" <> showDefault <> value 1)
        showServicesCommand = pure ShowServicesInfo
        createPeriodicallyCommand = Periodic
            <$> option auto (long "maxLimit" <> metavar "LIMIT" 
                            <> help "Create tickets periodically not exceeding some count")
        callArbitraryTicketCommand = CallArbitrary
            <$> option auto (long "callAny" <> metavar "TICKET_ID" 
                            <> help "Call any waiting ticket")


------------------------------- MAIN -----------------------------
withOptions :: ClientOptions -> Rdr a -> IO a
withOptions opts code = 
        withSession $ \sess -> do
            flip runReaderT (sess,clientConfig opts) $ do
                code

-- TODO : Add cmd parser to choose a functionality
mainProcessing :: IO ()
mainProcessing = do
    clientOpts <- execParser opts
    putStrLn $ "Starting Turnstat Client with :"  ++ (show $ clientConfig clientOpts)
    case clientCommand clientOpts of
        CreateTicket o sId -> do
            withOptions clientOpts $ do
                    api_key <- authenticate'
                    createTicket' api_key sId o
                    return ()
        CreateDuplicate o o2 sId -> requestSameTicket sId o o2
        --CreateDuplicate o o2 sId -> do 
            --withOptions clientOpts $ do
                --services <- getAllServices sess apiKey
                --putStrLn $ "Found " ++ (show $ length services) ++ " services."
                --mapM_ print services
                --let chosen = services !! sId
                --putStrLn $ "Using service: " ++ show chosen
                --let createTheTicket o = createTicket' apiKey (fromIntegral $ serviceID chosen) o >>= \p -> liftIO $ print p
                --forkIO $ runReaderT (createTheTicket o) (sess, cconfig)
                --runReaderT (createTheTicket o2) (sess, cconfig)
                --return ()
        CreateRandomTicket c -> do 
            withOptions clientOpts $ do
                    api_key <- authenticate'
                    createRandomTickets' api_key c
                    return ()
        ShowServicesInfo -> do
            withOptions clientOpts $ do
                    api_key <- authenticate'
                    services <- getAllServices' api_key
                    liftIO $ mapM_ print services
        -- Needs either an active turnstat or calling bots
        -- calls all tickets from a default service
        Periodic n -> do
            forever $ do
                withOptions clientOpts $ do
                    api_key <- authenticate'
                    liftIO $ print api_key
                    wt <- (sum . map snd) <$> allWaitingTickets'
                    if n < wt then createRandomTickets' api_key 1 else return ()
                        
        -- Calls and finishes a ticket right away
        CallArbitrary n -> do
            withOptions clientOpts $ do 
                    api_key <- authenticate'
                    slot <- setSlot 34 api_key 
                    liftIO $ putStrLn $ "Calling ticket from slot: " ++ slot
                    res  <- callArbitraryTicket api_key n
                    finishTicket api_key n
                    liftIO $ print res


-- | Querires all available services and creates the same ticket for a service chosen 
-- by an index
requestSameTicket :: Int -> Origin -> Origin -> IO ()
requestSameTicket n or1 or2= do
    withSession $ \sess -> do
        apiKey <- authenticate sess
        services <- getAllServices sess apiKey
        putStrLn $ "Found " ++ (show $ length services) ++ " services."
        mapM_ print services
        let chosen = services !! n
        putStrLn $ "Using service: " ++ show chosen
        forkIO $ createTicket sess apiKey (fromIntegral $ serviceID chosen) or1
        createTicket sess apiKey (fromIntegral $ serviceID chosen) or2


-- | Creates a random ticket depending on how many created are in line.
createTicketsPeriodically :: Session -> APIKey -> IO ()
createTicketsPeriodically sess api_key = do  
                print $ "received api key: " <> api_key  
                -- Gets an array of waiting tickets 
                numbs <- allWaitingTickets sess
                let ticketCount = sum numbs
                print $ "turnos en espera: " <> (pack $ show ticketCount)  
                r <- randomRIO (1,3) :: IO Int
                sleep 10
                if ticketCount < 101 
                    then 
                        createTicket sess (api_key) r GUIDED
                    else return () 


createRandomTickets' :: APIKey -> Int -> Rdr ()
createRandomTickets' api_key count = do  
                sess <- readSess
                liftIO $ print $ "received api key: " <> api_key  
                -- Gets an array of waiting tickets 
                numbs <- map snd <$> allWaitingTickets'
                allSerives <- getAllServices' api_key
                let servicesCount = length allSerives
                rs <- liftIO (randomRIO (0,servicesCount) :: IO Int)
                let randomService = allSerives !! rs
                r <- liftIO (randomRIO (0,fromEnum (maxBound :: Origin)) :: IO Int)
                let randomService = allSerives !! rs
                let randomOrigin = toEnum r :: Origin
                liftIO $ putStrLn $ "Using random service: " ++ (show randomService)
                replicateM_ count $ createTicket' api_key rs randomOrigin 
------------------------ API QUERYING FUNCTIONS --------------------

authenticate :: Session -> IO APIKey
authenticate sess = do 
        auth_r <- S.post sess login_url credentials
        let api_key = (auth_r ^. responseBody . key "user" . key "key" . _String)
        return api_key

loginURL = "login/auth.php"
authenticate' :: Rdr Text
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
        let wrap = ZipList . map cs
        let res = getZipList (TurnstatService <$> (readInt <$> ZipList servId) <*> (wrap names) <*> (wrap servChar) <*> (wrap enabled))
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
        let wrap = ZipList . map cs
        let res = getZipList (TurnstatService <$> (readInt <$> ZipList servId) <*> (wrap names) <*> (wrap servChar) <*> (wrap enabled))
        return (res)

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
createTicket' :: APIKey -> ServiceID -> Origin -> Rdr String
createTicket' api_key sId origin = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = ["service" := sId, "origin" := show origin]
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> createTicketURL) params
        liftIO $ print res
        let printable =  res ^. responseBody . key "printable" . _String
        return (cs printable)

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
--getWaitingTickets = error "Call ticket not implemented"
getWaitingTickets :: APIKey -> Rdr String
getWaitingTickets api_key = do
        sess <- readSess
        baseURL <- readBaseURL
        let params = [] :: [FormParam] 
        --res <- liftIO $ S.getWith (post_headers api_key) sess (baseURL <> getWaitingTicketsURL)
        res <- liftIO $ S.postWith (post_headers api_key) sess (baseURL <> getWaitingTicketsURL) params
        liftIO $ print res
        let ticketID =  res ^. responseBody . key "count_tickets" . key "count" . _String
        return (cs ticketID)


-- | sets the role for the current logged user
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

-- | Returns the sit on slot with id n and return the name of the slot.
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

post_headers :: APIKey -> Options
post_headers api_key = defaults & header "turnstat-api-key" .~ [cs api_key]
              -- & header "Content-Type" .~ [cs ("application/x-www-form-urlencoded"::String)]
              -- & auth ?~ basicAuth "usuario" "12345678"

------------------------ UTILITIES -------------------
sleep n = threadDelay $ n * 10 ^ 6 

-- authenticates for the first time: 

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


---------------------- CONSTANTS ----------------------
baseURLWithHost cc =  "http://" ++ (hostIP cc) ++ "/index.php/"
waitingListURL n = "ws/list_waiting_tickets_service_complete/" <> (show n)

base_url, login_url, create_ticket_url :: String 
--base_url = "http://172.16.0.4/index.php/"
base_url = "http://10.1.2.26/index.php/"
login_url = base_url <> "login/auth.php"

log_file = "logging.txt"

waiting_list_url:: Int -> String 
waiting_list_url n = base_url <> "ws/list_waiting_tickets_service_complete/" <> (show n)
create_ticket_url = base_url <> "ticket_management/create.php"
get_all_services_url = base_url <> "setup/services_get_all.php"

(user,pass) = ("usuario", "12345678") :: (String,String)

credentials :: [FormParam]
credentials = ["username" := user, "password" := pass]


{-
--------------------- Logging -------------------
-- | Logs all levels using a label l and a file f 
defLogger :: MonadIO m =>  FilePath -> String -> m (Logger String)
defLogger f l = makeDefaultLogger simpleTimeFormat (LogFile (FileLogSpec f 200 1) 3) levelDebug l

stdOutLogger :: MonadIO m =>  m (Logger String)
stdOutLogger =   makeDefaultLogger simpleTimeFormat' (LogStdout 4096) levelDebug "main"

getDefLogger :: IO (Logger String)
getDefLogger = defLogger "logging.txt" "turnstat"

-- | pass a info "something" or warning "something"  to this function
startLog :: LogT String IO b -> IO b
startLog ls = do
   logr <- getDefLogger 
   --runLogTSafe logr ls
   runLogT' logr ls


usingLogger :: Logger String -> LogT String IO b -> IO b
usingLogger lgr commands = runLogTSafe lgr commands
    
    
-}
