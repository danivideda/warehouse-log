module Module.Item where

data LogItem
    = Item
        { itemId :: Int
        , itemName :: String
        , storage :: Int
        , description :: String
        }
    | UnknownItem
    deriving (Show)

parseItem :: String -> [LogItem]
parseItem rawContent = map parseSingleItem (lines rawContent)

parseSingleItem :: String -> LogItem
parseSingleItem str = case words str of
    (i : n : s : d) -> makeItem i n s d
    _ -> UnknownItem

makeItem :: String -> String -> String -> [String] -> LogItem
makeItem itemId itemName storage description =
    Item
        { itemId = read itemId
        , itemName = itemName
        , storage = read storage
        , description = unwords description
        }