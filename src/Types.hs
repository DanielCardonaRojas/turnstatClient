module Types where

import Network.Wreq.Session (withAPISession, Session (..), withSession)
import Control.Monad.Reader
import Control.Monad.State
import qualified Control.Monad.State as ST
import Data.Text (Text (..), pack)
import Data.Time

---------------------- TYPES -----------------------
type APIKey = Text
type ServiceID = Int
type SlotID = Int
type TicketID = Int
data Origin = GUIDED | BUTTON | USER | FILAPP deriving (Show, Eq, Read, Enum, Bounded)
data Role = RUSER | AUDIT deriving (Show, Eq, Read, Enum, Bounded)
data Printable = Printable {printableLetter :: String, printableNumber :: Int} 

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
    hostCredentials :: (String, String) -- ^ User, Pass of the target TurnStat
  } deriving (Show,Eq)

defaultClientConfig = ClientConfig "172.16.0.4" ("usuario","12345678")

-- | The Command type, espeficies all the exposed functionality.
data Command 
    = CreateTicket Origin ServiceID
    | CreateDuplicate Origin Origin ServiceID -- ^ Creates to tickets at the same time for the same service
    | CreateRandomTicket ServiceID
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


-- | The data type that most querying functions will use is this function allows read acces to a global
-- Wreq Session and a ClientConfig tuple while doing IO
type Rdr a = ReaderT (Session, ClientConfig) IO a

--------------------------------- INSTANCES ---------------------

instance Show Printable  where
    show (Printable l n) = "Printable "  ++ l ++ "-" ++ (show n)

instance Read Printable where
    readsPrec _ str =  do
        (l,n) <- lex str 
        return (Printable l (read $ tail n), "") 
