module Main where

import Module.Item (parseItem)

main = do
    putStrLn "Warehouse Logging Software"
    items <- readFile "log/items.log"
    print $ parseItem items