data LogMessage = LogMessage
    { name :: String
    , id :: String
    , quantity :: Int
    , timestamp :: Int
    , status :: Status
    }

data Status = IN | OUT