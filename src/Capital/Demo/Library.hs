{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# LANGUAGE UndecidableInstances       #-}

module Capital.Demo.Library where

-- | An aggregate defines a source of truth regarding the state and behaviour of
-- some @BusinessModel@.

import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson                  as A
import qualified Data.ByteString.Base64.Lazy as B64
import           Data.ByteString.Lazy        (ByteString)
import           Data.Char                   (isHexDigit)
import           Data.List                   as List
import           Data.String
import qualified Data.Text                   as T
import           Data.Text.Lazy              as L
import qualified Data.Text.Lazy.Encoding     as LE
import           Data.Time





--------------------------------------------------------Business Model-------------------------------------------------------------------------------------
type EventVersion = Int


currentVersion :: EventVersion
currentVersion = 1

-- | Define "roots" of business model that can be affected by commands and
-- can generate events.
--
-- @BusinessModel@ are pure hence immutable data structures, whereas @Aggregate@s are
-- impure structures that maintain and control states.
class BusinessModel a where
  data Event a   :: *

  data Command a :: *

  data Error a :: *
  init :: a

  -- | Execute a command against this model and returns some event representing the
  -- outcome of the command
  act :: a -> Command a -> Either (Error a) (Event a)

  -- | Apply an event to the model resulting in a potentially new model
  apply :: a -> Event a -> a


-----------------------------------------Executor----------------------------------------------------------------------------------------------------------------------------------------
-- |Command execution for some `BusinessModel`, e.g. a fragment of state
--
class (BusinessModel a, EventClassifier s, ToJSON (EventType s)) => CommandExecutor a s | s -> a where
  getView :: s -> a
  getType :: a -> EventType s
  getEventType :: Event a -> EventType s
  setView :: (a -> s -> s)
  applyCommand :: (MonadIO m, MonadStore m, ToJSON (Event a))
                 => Command a
                 -> ServiceT (Error a) s m (Event a)
  applyCommand command = do
      v <- ask
      (ts, ev) <- liftIO $ do
        ts <- getCurrentTime
        ev <- atomically $ actAndApply v command
        return (ts, ev)
      let stored e etype = lift $ store (makeStoredEvent etype ts e)
      case ev of
       Right (e, etype) -> stored e etype >> return e
       Left l  -> throwError l


currentSha1 :: Encoded Hex
currentSha1 = "000000000000000000"

makeStoredEvent ::  ToJSON (Event a)
                   => EventType s
                   -> UTCTime
                   -> Event a
                   -> StoredEvent s
makeStoredEvent etype ts = StoredEvent currentVersion etype ts currentSha1 . encode

data StoredEvent s = StoredEvent { eventVersion :: EventVersion   -- ^Version of this event, useful to support migration and graceful upgrades of events
                                 , eventType    :: EventType s    -- ^Type of event, needed to properly deserialize the @event@ when needed
                                 , eventDate    :: UTCTime           -- ^Timestamp for this event
                                 , eventSHA1    :: Encoded Hex    -- ^Current source code version at time of event
                                 , event        :: ByteString     -- ^Payload
                                 }

instance Show (EventType s) => Show (StoredEvent s) where
  show (StoredEvent v t d s ev) = "StoredEvent " ++ show v ++ " " ++ show t ++ " " ++ show d ++ " " ++ show s ++ " " ++ show ev

instance Eq (EventType s) => Eq (StoredEvent s) where
  (StoredEvent v t d s ev) == (StoredEvent v' t' d' s' ev') =  v == v' && t == t' && d == d' && s == s' && ev == ev'

instance ToJSON (EventType s) =>  ToJSON (StoredEvent s) where
  toJSON (StoredEvent v t d s p) = object [
    "eventVersion"    .= v,
    "eventType"       .= t,
    "eventDate"       .= d,
    "eventSHA1"       .= s,
    "event"           .= A.String  (L.toStrict $ encodedText $ toBase64Text p)
    ]

instance FromJSON (EventType s) => FromJSON (StoredEvent s) where
  parseJSON (Object o) = StoredEvent
                             <$> o .: "eventVersion"
                             <*> o .: "eventType"
                             <*> o .: "eventDate"
                             <*> o .: "eventSHA1"
                             <*> ((fromBase64Text . Encoded . L.fromStrict) <$>  o .: "event")
  parseJSON _          = mempty


-- | Provides a way to classify events according to some category.
-- Instances of this class are expected to be receptacles for stream of events that affect their
-- state.
class (Show (EventType c), Enum (EventType c)) => EventClassifier c where
  data EventType c :: *
  applyEvent :: c -> StoredEvent c -> c


actAndApply :: (CommandExecutor a s) => TVar s -> Command a -> STM (Either (Error a) (Event a, EventType s))
actAndApply v command = do
  s <- readTVar v
  let view = getView s
  let e  = view `act` command
  case e of
   Right ev -> do
     let newView = view `apply` ev
     modifyTVar' v (setView newView)
     return $ Right (ev, getEventType ev)
   Left ev -> return $ Left ev

gets :: (MonadIO m) => (s -> b) -> ServiceT e s m b
gets f = f <$> (ask >>= liftIO . readTVarIO)

modify :: (MonadIO m) => (s -> s) -> ServiceT e s m ()
modify f = ask >>= liftIO . atomically . flip modifyTVar' f

newtype ServiceT e s m a =
  ServiceT { runServiceT :: ExceptT e (ReaderT (TVar s) m) a }
  deriving (Functor, Applicative, Monad, Alternative, MonadPlus, MonadError e, MonadReader (TVar s))

instance MonadTrans (ServiceT e r) where
  lift = ServiceT . lift . lift

instance (MonadIO m) => MonadIO (ServiceT e s m) where
  liftIO = lift . liftIO

runService :: (Monad m) => ServiceT e s m a -> TVar s -> m (Either e a)
runService effect = runReaderT (runExceptT . runServiceT $ effect)
--------------------------------------------MonadStore-------------------------------------------------------------------------------------------

class (MonadIO m) => MonadStore m where
  store :: ToJSON a => a -> m ()
  load :: FromJSON a => m x -> IO [a]

-------------------------------------------------------------- Encoding ---------------------------------------------------------------------------
data Base64
data Hex

newtype Encoded code = Encoded { encodedText :: Text } deriving (Eq, Ord)

instance Show (Encoded s) where
  show (Encoded t) = show t

instance Read (Encoded s) where
  readsPrec n = List.map ( \ (t,s) -> (Encoded t, s)) . readsPrec n

instance ToJSON (Encoded Hex) where
  toJSON (Encoded t) = String $ L.toStrict t

instance FromJSON (Encoded Hex) where
  parseJSON (String t) = if T.all isHexDigit t
                         then return $ Encoded (L.fromStrict t)
                         else fail $ "not a valid hexadecimal encoded string: " ++ show t
  parseJSON v          = fail $ "not a valid hexadecimal encoded string: "  ++ show v

instance IsString (Encoded Hex) where
  -- not quite correct
  fromString = Encoded . L.pack

toBase64Text :: ByteString -> Encoded Base64
toBase64Text = Encoded . LE.decodeUtf8 . B64.encode

fromBase64Text :: Encoded Base64 -> ByteString
fromBase64Text = B64.decodeLenient . LE.encodeUtf8 . encodedText

encodeBase64 :: ByteString -> ByteString
encodeBase64 = B64.encode
