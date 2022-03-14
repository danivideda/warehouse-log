module Message where

import Data.Time
import Data.Time.Clock.POSIX (getPOSIXTime)
import Module.Item (Item, parseItem)
import Module.Person (Person)

data LogMessage
    = LogMessage
        { item :: Item
        , person :: Person
        , quantity :: Int
        , timestamp :: Int
        , status :: Status
        }
    | Unknown
    deriving (Show)

data Status = IN | OUT deriving (Show)
