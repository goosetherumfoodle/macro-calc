module Calculator (main) where

import System.Environment (getArgs)

-- 1. read in target and actual numbers
-- 2. add up actual numbers
-- 3. compare totals to target
-- 4. print comparison

main :: IO ()
main =  fileContent >>= putStrLn

fileContent :: IO String
fileContent = filePath >>= readFile

filePath :: IO String
filePath = fmap (flip (!!) 0) getArgs
