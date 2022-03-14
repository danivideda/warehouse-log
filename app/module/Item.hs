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
    putStrLn "You're about to add new item into the inventory, please fill the information below: \n"
    name <- prompt "Item name: "
    description <- prompt "Description: "

    let lastId = itemId $ last oldLogItemList
        newId = lastId + 1
        newLogItem =
            LogItem
                { itemId = newId
                , itemName = name
                , storage = 0
                , description = description
                }
        newLogItemList = oldLogItemList ++ [newLogItem]
    parseLogItem newLogItemList
    putStrLn $ "Successfully added new item: " ++ name
    emptyPrompt <- prompt "Press enter to continue."
    return newLogItemList

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
    let parsedLogItem = convertToLog logItemList
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