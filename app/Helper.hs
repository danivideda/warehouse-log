module Helper where

import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine