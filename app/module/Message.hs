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

data Status = IN | OUT | NEW | REMOVE | ERR deriving (Show, Read)

secondSinceEpoch :: UTCTime -> Int
secondSinceEpoch =
    floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

makeLogMessage :: LogItem -> String -> IO LogMessage
makeLogMessage item status = do
    u <- getCurrentTime
    let currentTime = secondSinceEpoch u
        message =
            if item == UnknownItem
                then
                    LogMessage
                        { item = 0
                        , quantity = 0
                        , timestamp = currentTime
                        , status = ERR
                        }
                else
                    LogMessage
                        { item = itemId item
                        , quantity = storage item
                        , timestamp = currentTime
                        , status = read status :: Status
                        }
    return message

parseLogMessage :: LogMessage -> IO ()
parseLogMessage message = do
    u <- getCurrentTime
    let currentTime = secondSinceEpoch u
    let parsedLogMessage =
            "ItemID: "
                ++ show (item message)
                ++ " Quantity: "
                ++ show (quantity message)
                ++ " Timestamp: "
                ++ show (currentTime)
                ++ " Status: "
                ++ show (status message)
                ++ "\n"
    appendFile "log/messages.log" parsedLogMessage
