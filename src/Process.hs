{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Process where

import Network.Wreq
import Network.Wreq.Session (Session (..), withSession)

import Data.Text (Text (..), pack)
import Data.Monoid
import System.Random
import Data.String.Conversions

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State as ST
import Control.Monad.Trans
import Control.Applicative

--import Control.Monad.Log
--import Control.Monad.Log.Label

import Options.Applicative
import Control.Monad

import Types
import Client
import CmdParser

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
    putStrLn $ "Starting Turnstat Client with: "  ++ (show $ clientConfig clientOpts)
    case clientCommand clientOpts of
        CreateTicket o sId -> do
            withOptions clientOpts $ do
                api_key <- authenticate'
                t <- createTicket' api_key sId o
                liftIO $ print t

        CreateDuplicate o o2 sId -> do
           (t1, t2) <-  requestSameTicket' (clientConfig clientOpts) sId o o2
           putStrLn $ "First " ++ show t1 ++ " second " ++ show t2

        CreateRandomTicket c -> do 
            withOptions clientOpts $ do
                    api_key <- authenticate'
                    ts <- createRandomTickets' api_key c
                    liftIO $ putStrLn "Created random tickets: "
                    liftIO (mapM_ print ts)

        ShowInfo x -> do
            case x of 
                ServicesInfo ->  do 
                    withOptions clientOpts $ do
                            api_key <- authenticate'
                            services <- getAllServices' api_key
                            liftIO $ mapM_ print services
                SlotsInfo ->
                    withOptions clientOpts $ do
                            api_key <- authenticate'
                            slots <- getAllSlots api_key
                            liftIO $ mapM_ print slots
                UsersInfo -> 
                    withOptions clientOpts $ do
                            api_key <- authenticate'
                            users <- getAllUsers api_key
                            liftIO $ mapM_ print users

        -- Needs either an active turnstat or calling bots
        -- calls all tickets from a default service
        Periodic n -> do
            forever $ do
                withOptions clientOpts $ do
                    api_key <- authenticate'
                    liftIO $ print api_key
                    wt <- (sum . map snd) <$> allWaitingTickets'
                    if n < wt then createRandomTickets' api_key 1 >> return () else return ()
                        
        -- Calls and finishes a ticket right away
        CallArbitrary n -> do
            withOptions clientOpts $ do 
                    api_key <- authenticate'
                    allSlotsIDs <- (map slotID . filter slotEnabled) <$> getAllSlots api_key
                    someSlot <- liftIO $ chooseRIO allSlotsIDs
                    slotName <- setSlot someSlot api_key 
                    liftIO $ putStrLn $ "Calling ticket from slot: " ++ slotName
                    res  <- callArbitraryTicket api_key n
                    finishTicket api_key n
                    liftIO $ print res

        PrintTicket tid pid -> do
            putStrLn $ "Printing ticket: " ++ (show tid) ++ " using printer: " ++ (show pid)
            withOptions clientOpts $ do 
                    api_key <- authenticate'
                    res <- printTicket tid pid api_key
                    liftIO $ print res

        CreateUser name pass role -> do
            putStrLn $ "Creating user with name: " ++ (show name) ++ " role: " ++ (show role)
            withOptions clientOpts $ do 
                    api_key <- authenticate'
                    res <- createUser name pass role api_key
                    liftIO $ print res


-- | requestSameTicket' tries to enqueue to tickets at the same time to see if the resulting printables repeat
-- this is used to test TurnStat is working right.
--requestSameTicket' :: ClientConfig -> ServiceID -> Origin -> Origin -> IO (Printable, Printable)
requestSameTicket' :: ClientConfig -> ServiceID -> Origin -> Origin -> IO (TurnstatTicket, TurnstatTicket)
requestSameTicket' cconfig sId o o2= do
            withSession $ \sess -> do
                apiKey <- authenticate sess
                services <- getAllServices sess apiKey
                putStrLn $ "Found " ++ (show $ length services) ++ " services."
                mapM_ print services
                let chosen = services !! sId
                putStrLn $ "Using service: " ++ show chosen
                m <- newEmptyMVar
                let createTheTicket o = createTicket' apiKey (fromIntegral $ serviceID chosen) o
                let createTheTicketOrg org = runReaderT (createTheTicket org) (sess, cconfig)
                forkIO $ createTheTicketOrg o >>= putMVar m
                r <- createTheTicketOrg o2
                x <- takeMVar m
                return (x,r)

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


createRandomTickets' :: APIKey -> Int -> Rdr [TurnstatTicket]
createRandomTickets' api_key count = do  
                sess <- readSess
                liftIO $ print $ "received api key: " <> api_key  
                allSerives <- filter serviceEnabled <$> getAllServices' api_key
                let servicesCount = length allSerives
                let randomsRIO (a,b) = replicateM count $ randomRIO (a,b)
                rs <- liftIO (randomsRIO (0,servicesCount - 1) :: IO [Int])
                ro <- liftIO (randomsRIO (0,fromEnum (maxBound :: Origin)) :: IO [Int])
                let randomService = map (fromIntegral . serviceID . (allSerives !!)) rs
                let randomOrigin = map toEnum ro :: [Origin]
                liftIO $ putStrLn $ "Using random service: " ++ (show randomService)
                mapM (uncurry $ createTicket' api_key) (zip randomService randomOrigin)


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
