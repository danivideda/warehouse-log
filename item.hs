data Item = Item
    { name :: String
    , id :: Int
    , properties :: [Property]
    }

data Property = Fragile | Wet