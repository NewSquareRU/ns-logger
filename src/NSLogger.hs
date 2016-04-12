{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module NSLogger
       (logNS, Severity(..), logNSINFO, logNSDEBUG, logNSERROR)
       where


import Data.ByteString.Lazy (ByteString)
import Control.Monad.IO.Class (MonadIO)
import Data.Time
       (UTCTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)
import Control.Monad.Log
       (WithTimestamp(..), WithSeverity(..), Severity(..), MonadLog)

import qualified Control.Monad.Log as Log


logNS
    :: (ToJSON a, MonadIO m, MonadLog (WithTimestamp (WithSeverity a)) m)
    => UTCTime -> Severity -> a -> m ()
logNS timestamp severity' msg =
    Log.logMessage
        (WithTimestamp
         { discardTimestamp = (WithSeverity
                               { discardSeverity = msg
                               , msgSeverity = severity'
                               })
         , msgTimestamp = timestamp
         })


logNSSeverity
    :: (ToJSON a, MonadIO m, MonadLog (WithTimestamp (WithSeverity a)) m)
    => Severity -> a -> m ()
logNSSeverity severity' msg =
    Log.timestamp
        (WithSeverity
         { discardSeverity = msg
         , msgSeverity = severity'
         }) >>=
    Log.logMessage


logNSINFO
    :: (ToJSON a, MonadIO m, MonadLog (WithTimestamp (WithSeverity a)) m)
    => a -> m ()
logNSINFO = logNSSeverity Informational


logNSDEBUG
    :: (ToJSON a, MonadIO m, MonadLog (WithTimestamp (WithSeverity a)) m)
    => a -> m ()
logNSDEBUG = logNSSeverity Debug


logNSERROR
    :: (ToJSON a, MonadIO m, MonadLog (WithTimestamp (WithSeverity a)) m)
    => a -> m ()
logNSERROR = logNSSeverity Error

data NSLogLine a = NSLogLine
    { severity :: String
    , time :: String
    , logMessage :: a
    } deriving (Generic)

instance ToJSON a => ToJSON (NSLogLine a)

renderNSLog :: (ToJSON a) => WithTimestamp (WithSeverity a) -> ByteString
renderNSLog msg =
    encode $
    NSLogLine
    { severity = severity2nsseverity (msgSeverity (discardTimestamp msg))
    , time = utc2unixtimestamp (msgTimestamp msg)
    , logMessage = discardSeverity (discardTimestamp msg)
    }

utc2unixtimestamp :: UTCTime -> String
utc2unixtimestamp = formatTime defaultTimeLocale "%s"


severity2nsseverity :: Severity -> String
severity2nsseverity sevrty =
    case sevrty of
        Informational -> "INFO"
        Debug -> "DEBUG"
        Error -> "ERROR"
        _ -> show sevrty
