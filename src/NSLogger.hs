{-# LANGUAGE DeriveGeneric #-}

module NSLogger
       (NSLogStr(..), logNS, Severity(..), logNSINFO, logNSDEBUG, logNSERROR)
       where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)
import System.Log.FastLogger (ToLogStr(..), LogStr)


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


logNS :: ToJSON a => UTCTime -> Severity -> a -> LogStr
logNS timestamp severity' msg = toLogStr (NSLogStr timestamp severity' msg)


logNSSeverity
    :: (MonadIO m, ToJSON a)
    => Severity -> a -> m LogStr
logNSSeverity severity' msg =
    liftIO getCurrentTime >>=
    \t ->
         return (logNS t severity' msg)


logNSINFO
    :: (MonadIO m, ToJSON a)
    => a -> m LogStr
logNSINFO = logNSSeverity INFO


logNSDEBUG
    :: (MonadIO m, ToJSON a)
    => a -> m LogStr
logNSDEBUG = logNSSeverity DEBUG


logNSERROR
    :: (MonadIO m, ToJSON a)
    => a -> m LogStr
logNSERROR = logNSSeverity ERROR
