{-# LANGUAGE DeriveGeneric #-}

module NSLogger
       (NSLogStr(..), logNSDebug, logNSInfo, logNSSeverity, Severity(..))
       where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)
import System.Log.FastLogger (ToLogStr(..))


data Severity
    = DEFAULT
    | DEBUG
    | INFO
    | NOTICE
    | WARNING
    | ERROR
    | CRITICAL
    | ALERT
    | EMERGENCY
    deriving (Eq,Ord,Show,Generic)

instance ToJSON Severity


data NSLogStr a = NSLogStr
    { time :: UTCTime
    , severity :: Severity
    , logMessage :: a
    } deriving (Eq,Ord,Show,Generic)


instance ToJSON a => ToJSON (NSLogStr a)


instance ToJSON a => ToLogStr (NSLogStr a) where
  toLogStr = toLogStr . encode


logNS :: ToJSON a => UTCTime -> Severity -> a -> NSLogStr a
logNS = NSLogStr


logNSSeverity :: (ToJSON a, MonadIO m) => Severity -> a -> m (NSLogStr a)
logNSSeverity severity msg = do
   ut <- liftIO getCurrentTime
   return (logNS ut severity msg)


logNSDebug :: (ToJSON a, MonadIO m) => a -> m (NSLogStr a)
logNSDebug = logNSSeverity DEBUG

logNSInfo :: (ToJSON a, MonadIO m) => a -> m (NSLogStr a)
logNSInfo = logNSSeverity INFO
