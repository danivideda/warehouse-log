module Item where

data Item = Item
    { name :: String
    , itemId :: Int
    , properties :: [Property]
    }
    deriving (Show)

data Property = Fragile | Wet deriving (Show)

items :: [Item]
items =
    [ Item{name = "Chair", itemId = 1, properties = [Fragile, Wet]}
    , Item{name = "Table", itemId = 2, properties = [Fragile, Wet]}
    ]