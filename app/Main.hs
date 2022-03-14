module Main where

import Helper (prompt)
import Module.Item (LogItem, addNewItem, description, itemId, itemName, parseItem, storage)
import Module.Message (LogMessage)

showItem :: [LogItem] -> String
showItem [] = replicate 58 '='
showItem (item : rest) =
    "ID: " ++ show (itemId item)
        ++ "\nName: "
        ++ itemName item
        ++ "\nStorage: "
        ++ show (storage item)
        ++ "\nDescription: "
        ++ description item
        ++ "\n-----------------------------\n"
        ++ showItem rest

runProgram :: [LogItem] -> [LogMessage] -> IO ()
runProgram items messages = do
    putStrLn $ showItem items
    choice <- prompt "(a) Restock item  (b) Remove item (c) Add new item\n"
    case choice of
        "a" -> putStrLn "You choose A"
        "b" -> putStrLn "You choose B"
        "c" -> do
            newItems <- addNewItem items
            runProgram newItems messages
        _ -> do
            empty <- prompt "Wrong input! Press enter to try again."
            runProgram items messages

main = do
    items <- fmap parseItem (readFile "log/items.log")
    -- messages <- fmap parseMessage (readFile "log/messages.log")
    putStrLn "=============== Warehouse Logging Software ==============="
    runProgram items []