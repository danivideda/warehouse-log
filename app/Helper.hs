module Helper where

prompt :: String -> IO String
prompt text = do
    putStr text
    getLine