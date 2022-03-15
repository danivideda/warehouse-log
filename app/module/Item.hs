module Module.Item where

import Helper (prompt)

data LogItem
    = LogItem
        { itemId :: Int
        , itemName :: String
        , storage :: Int
        , description :: String
        }
    | UnknownItem
    deriving (Show)

addNewItem :: [LogItem] -> IO [LogItem]
addNewItem oldLogItemList = do
    putStrLn "\nYou're about to add new item into the inventory, please fill the information below: "
    name <- prompt "Item name: "
    storage <- prompt "Quantity: "
    description <- prompt "Description: "

    let lastId = itemId $ last oldLogItemList
        newId = lastId + 1
        newLogItem =
            LogItem
                { itemId = newId
                , itemName = name
                , storage = read storage
                , description = description
                }
    let newLogItemList = oldLogItemList ++ [newLogItem]
    -- parseLogItem newLogItemList
    return newLogItemList

restockItem :: [LogItem] -> Int -> IO [LogItem]
restockItem logItemList choiceId = do
    return logItemList

parseLogItem :: [LogItem] -> IO ()
parseLogItem logItemList = do
    let convertToLog :: [LogItem] -> String
        convertToLog [] = ""
        convertToLog (logItem : rest) =
            show (itemId logItem)
                ++ " "
                ++ itemName logItem
                ++ " "
                ++ show (storage logItem)
                ++ " "
                ++ description logItem
                ++ "\n"
                ++ convertToLog rest
    let parsedLogItem = init $ convertToLog logItemList -- using init to remove the last \n at the end of the .log
    writeFile "log/items.log" parsedLogItem

parseItem :: String -> [LogItem]
parseItem rawContent = map parseSingleItem (lines rawContent)

parseSingleItem :: String -> LogItem
parseSingleItem str = case words str of
    (i : n : s : d) -> makeItem i n s d
    _ -> UnknownItem

makeItem :: String -> String -> String -> [String] -> LogItem
makeItem itemId itemName storage description =
    LogItem
        { itemId = read itemId
        , itemName = itemName
        , storage = read storage
        , description = unwords description
        }