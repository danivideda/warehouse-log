module Main where

import Module.Item (Item, description, itemId, itemName, parseItem, storage)

prompt :: String -> IO String
prompt text = do
    putStr text
    getLine

showItem :: [Item] -> String
showItem [] = ""
showItem (item : rest) =
    "\nID: " ++ show (itemId item)
        ++ "\nName: "
        ++ itemName item
        ++ "\nStorage: "
        ++ show (storage item)
        ++ "\nDescription: "
        ++ description item
        ++ "\n-----------------------------"
        ++ showItem rest

main = do
    putStrLn "=============== Warehouse Logging Software ==============="
    items <- fmap parseItem (readFile "log/items.log")
    putStrLn $ showItem items