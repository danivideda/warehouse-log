module Main where

import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import Control.Monad.Trans.Writer (WriterT, execWriterT, runWriterT, tell)
import Helper (prompt)
import Module.Item (LogItem, addNewItem, description, itemId, itemName, parseItem, parseLogItem, storage)
import Module.Message (LogMessage, makeLogMessage, parseLogMessage)

runProgram :: [LogItem] -> [LogMessage] -> IO ()
runProgram items messages = do
    putStrLn $ replicate 58 '='
    putStrLn $ showItem items
    choice <- prompt "(a) Show all item  (b) Restock item  (c) Remove item  (d) Add new item  (e) Exit program\n"
    case choice of
        "a" -> do
            putStrLn $ showAllItem items
            empty <- prompt "Press enter to go back"
            runProgram items messages
        "b" -> putStrLn "RESTOCK ITEM -TODO-"
        "c" -> putStrLn "REMOVE ITEM -TODO-"
        "d" -> do
            newItems <- addNewItem items
            parseLogItem newItems
            logMessage <- makeLogMessage (last newItems) "IN"
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