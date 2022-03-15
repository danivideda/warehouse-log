module Module.Message where

import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Module.Item (LogItem, parseItem)
import Module.Person (Person)

data LogMessage
    = LogMessage
        { item :: LogItem
        , person :: Person
        , quantity :: Int
        , timestamp :: Int
        , status :: Status
        }
    | Unknown
    deriving (Show)

data Status = IN | OUT deriving (Show)