module Main where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Data.List
import Helper (MaybeT, liftMaybeT, maybeReadInt, prompt, runMaybeT)
import Module.Item (LogItem (UnknownItem), addNewItem, description, itemId, itemName, parseItem, parseLogItem, restockItem, storage, takeItem)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)
import System.IO (hFlush, stdout)

runProgram :: [LogItem] -> [LogMessage] -> IO ()
runProgram items messages = do
    putStrLn $ replicate 58 '='
    putStrLn $ showItem items
    choice <- prompt "(a) Show all item  (b) Restock item  (c) Take item  (d) Add new item  (e) Exit program\n"
    case choice of
        "a" -> do
            putStrLn $ showAllItem items
            empty <- prompt "Press enter to go back"
            runProgram items messages
        "b" -> do
            putStrLn "You're about to restock an item: "
            -- Insert ItemID
            putStr "Insert ItemID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            -- Insert Amount
            putStr "Insert amount to restock: "
            hFlush stdout
            amount <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            newRestockedItems <- restockItem items choice amount
            parseLogItem newRestockedItems
            let changedItem = find (\item -> itemId item == choice) newRestockedItems
                extractItem :: Maybe LogItem -> LogItem
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            logMessage <- makeLogMessage ((extractItem changedItem){storage = amount}) "IN"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Press enter to continue."
            runProgram newRestockedItems messages
        "c" -> do
            putStrLn "You're about to take out an item: "
            -- Insert ItemID
            putStr "Insert ItemID: "
            hFlush stdout
            choice <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0
            -- Insert Amount
            putStr "Insert amount to take: "
            hFlush stdout
            amount <- do
                result <- runMaybeT maybeReadInt
                case result of
                    (Just a) -> return a
                    Nothing -> return 0

            updatedItems <- takeItem items choice amount
            parseLogItem updatedItems

            let changedItem = find (\item -> itemId item == choice) updatedItems
                extractItem :: Maybe LogItem -> LogItem
                extractItem (Just a) = a
                extractItem Nothing = UnknownItem

            logMessage <- makeLogMessage ((extractItem changedItem){storage = amount}) "OUT"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Press enter to continue."
            runProgram updatedItems messages
        "d" -> do
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
            newItems <- addNewItem items name storage description
            parseLogItem newItems
            logMessage <- makeLogMessage (last newItems) "NEW"
            parseLogMessage logMessage
            emptyPrompt <- prompt "Successfully added new item! Press enter to continue."
            runProgram newItems messages
        "e" -> putStrLn "Goodbye!"
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram items messages

showItem :: [LogItem] -> String
showItem items = showItemFunc (length items) (take 2 items)
  where
    showItemFunc count [] = case count of
        0 -> "The item list is currently empty.\n" ++ replicate 58 '='
        1 -> "\n" ++ replicate 58 '='
        2 -> "\n" ++ replicate 58 '='
        _ -> "...and " ++ show (count - 2) ++ " more." ++ "\n" ++ replicate 58 '='
    showItemFunc count (item : rest) =
        "ID: " ++ show (itemId item)
            ++ "\nName: "
            ++ itemName item
            ++ "\nStorage: "
            ++ show (storage item)
            ++ "\nDescription: "
            ++ description item
            ++ "\n"
            ++ replicate 29 '-'
            ++ "\n"
            ++ showItemFunc count rest

showAllItem :: [LogItem] -> String
showAllItem [] = replicate 58 '='
showAllItem (item : rest) =
    "ID: " ++ show (itemId item)
        ++ "\nName: "
        ++ itemName item
        ++ "\nStorage: "
        ++ show (storage item)
        ++ "\nDescription: "
        ++ description item
        ++ "\n"
        ++ replicate 29 '-'
        ++ "\n"
        ++ showAllItem rest

main :: IO ()
main = do
    items <- fmap parseItem (readFile "log/items.log")
    -- messages <- fmap parseMessage (readFile "log/messages.log")
    putStrLn "\n\n=============== Warehouse Logging Software ==============="
    runProgram items []