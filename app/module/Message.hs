module Module.Message where

import Data.Int
import Data.Time
import Data.Time.Clock.POSIX
import Module.Item

data LogMessage
    = LogMessage
        { item :: Int
        , quantity :: Int
        , timestamp :: Int
        , status :: Status
        }
    | Unknown
    deriving (Show)

data Status = IN | OUT deriving (Show, Read)

nanosSinceEpoch :: UTCTime -> Int
nanosSinceEpoch =
    floor . (1e6 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

parseLogMessage :: LogMessage -> IO ()
parseLogMessage message = do
    u <- getCurrentTime
    let currentTime = nanosSinceEpoch u
    let parsedLogMessage =
            show (item message)
                ++ " Quantity: "
                ++ show (quantity message)
                ++ " Timestamp: "
                ++ show (currentTime)
                ++ " Status: "
                ++ show (status message)
                ++ "\n"

    appendFile "log/messages.log" parsedLogMessage

makeLogMessage :: LogItem -> String -> IO LogMessage
makeLogMessage item status = do
    u <- getCurrentTime
    let currentTime = nanosSinceEpoch u
        message =
            LogMessage
                { item = itemId item
                , quantity = storage item
                , timestamp = currentTime
                , status = read status :: Status
                }
    return message