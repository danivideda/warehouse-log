module Module.Item where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import System.IO (hFlush, stdout)

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
    putStr "Quantity: "
    hFlush stdout
    storage <- do
        result <- runMaybeT maybeReadInt
        case result of
            (Just a) -> return a
            Nothing -> return 0
    description <- prompt "Description: "

    let lastId = itemId $ last oldLogItemList
        newId = lastId + 1
        newLogItem =
            LogItem
                { itemId = newId
                , itemName = name
                , storage = storage
                , description = description
                }
    let newLogItemList = oldLogItemList ++ [newLogItem]
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