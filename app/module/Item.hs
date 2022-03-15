module Module.Item where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
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
    deriving (Show, Eq)

addNewItem :: [LogItem] -> String -> Int -> String -> IO [LogItem]
addNewItem oldLogItemList name storage description = do
    let lastId =
            if null oldLogItemList
                then 0
                else itemId $ last oldLogItemList
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

restockItem :: [LogItem] -> Int -> Int -> IO [LogItem]
restockItem oldLogItemList choice amount = do
    let itemExist = find (\item -> (itemId item) == choice) oldLogItemList

        extractItem :: Maybe LogItem -> LogItem
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        replaceItem :: [LogItem] -> LogItem -> Int -> [LogItem]
        replaceItem [] chosenItem amount = []
        replaceItem (item : rest) chosenItem amount
            | item == chosenItem = [item{storage = storage item + amount}] ++ replaceItem rest chosenItem amount
            | otherwise = [item] ++ replaceItem rest chosenItem amount

    let restockedLogItemList =
            if (extractItem itemExist) == UnknownItem
                then oldLogItemList
                else replaceItem oldLogItemList (extractItem itemExist) amount

    if (extractItem itemExist) == UnknownItem
        then putStrLn "Unknown item inserted"
        else putStrLn "Successfully restocked item!"

    return restockedLogItemList

takeItem :: [LogItem] -> Int -> Int -> IO [LogItem]
takeItem oldLogItemList choice amount = do
    let itemExist = find (\item -> (itemId item) == choice) oldLogItemList

        extractItem :: Maybe LogItem -> LogItem
        extractItem (Just a) = a
        extractItem Nothing = UnknownItem

        replaceItem :: [LogItem] -> LogItem -> Int -> [LogItem]
        replaceItem [] chosenItem amount = []
        replaceItem (item : rest) chosenItem amount
            | item == chosenItem = [item{storage = storage item - amount}] ++ replaceItem rest chosenItem amount
            | otherwise = [item] ++ replaceItem rest chosenItem amount

    let updatedLogItemList =
            if (extractItem itemExist) == UnknownItem
                then oldLogItemList
                else
                    if amount > (storage $ extractItem itemExist)
                        then oldLogItemList
                        else replaceItem oldLogItemList (extractItem itemExist) amount

    if (extractItem itemExist) == UnknownItem
        then putStrLn "Unknown item inserted"
        else
            if amount > (storage $ extractItem itemExist)
                then putStrLn "Not enough storage quantity to take items."
                else putStrLn "Successfully took item!"

    return updatedLogItemList

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