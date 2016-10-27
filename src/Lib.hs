{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- Maybe? {-# LANGUAGE OverloadedLists #-}

module Lib where

import Network.Wreq
import Network.Wreq.Session (Session (..), withSession)

import Data.Text (Text (..), pack)
import Data.Monoid
import System.Random
import Data.String.Conversions

--import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State as ST
import Control.Monad.Trans
import Control.Applicative

--import Control.Monad.Log
--import Control.Monad.Log.Label


import Options.Applicative
import Data.Monoid
import Control.Monad

import Types
import Client
import CmdParser
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

GENERATE DOCUMENTATION:
    stack haddock

This will leave documentation somewhere arount: 
    .stack-work/dist/x86_64-osx/Cabal-1.24.0.0/doc/html/turnstatClient/index.html
-}

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

        CreateDuplicate o o2 sId -> do
           (t1, t2) <-  requestSameTicket' (clientConfig clientOpts) sId o o2
           putStrLn $ "First ticket " ++ show t1 ++ " second ticket " ++ show t2

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


-- | requestSameTicket' tries to enqueue to tickets at the same time to see if the resulting printables repeat
-- this is used to test TurnStat is working right.
requestSameTicket' :: ClientConfig -> ServiceID -> Origin -> Origin -> IO (Printable, Printable)
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
                let createTheTicketOrg org = runReaderT (createTheTicket org) (sess, cconfig) >>= \t -> putMVar m (read t)
                forkIO $ createTheTicketOrg o
                createTheTicketOrg o2
                x <- takeMVar m
                r <- takeMVar m
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
