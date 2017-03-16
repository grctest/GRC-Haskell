-- cabal repl

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Maybe
--import Data.List
import Text.HTML.Scalpel
import Data.Tuple.Select
import Data.List

type BlockNumber = Integer
type BlockDiff = Integer
type QuantityBlocks = Integer
counterInit = 0

main :: IO ()
main = do
 foo <- putStrLn "Input CPID!"
 inputCPID <- getLine
 
 foo2 <- putStrLn "How many blocks in a row do you want to find?"
 userBlockQuery <- getLine

 scrapedCPIDs <- mapM (\x -> scrapeURL x scrapeCPIDTable) [(formingURL inputCPID)]

 let blockNums = concat(fromJust (head scrapedCPIDs))
 let sequentialBlockList = createLists blockNums
 let output = filterSequencesOnUserInput (read userBlockQuery) sequentialBlockList
 print output

formingURL :: String -> String
formingURL input = "https://gridcoinstats.eu/cpid.php?a=view&id=" ++ input

scrapeCPIDTable :: Scraper String [[BlockNumber]]
scrapeCPIDTable = chroots ("td" @: ["style" @= "text-align: center"]) scrapeCPID

scrapeCPID :: Scraper String [BlockNumber]
scrapeCPID = do
    cpid <- text $ "a"
    return [read cpid]

createLists :: [BlockNumber] -> [[BlockNumber]]
createLists [] = []
createLists (x:xs) = do
    if (xs == [])
        then do
            []
        else do
            if (((head(xs)) - x) == 1)
              then do
                let sequenctialChunk = splitAt (fromJust(quantitySequentialBlocks (x:xs) counterInit)) xs
                let currentChunk = fst sequenctialChunk
                let recursiveChunk = snd sequenctialChunk
                if (recursiveChunk /= [])
                  then do
                    [[x] ++ currentChunk] ++ createLists recursiveChunk
                  else do
                    [[x] ++ currentChunk]
              else do
                createLists xs

quantitySequentialBlocks :: [BlockNumber] -> Int -> Maybe Int
quantitySequentialBlocks (x:xs) counter = do
    if (((head(xs)) - x) == 1)
      then do
        quantitySequentialBlocks xs (counter + 1)
      else do
        return (counter)

filterSequencesOnUserInput :: Int -> [[BlockNumber]] -> [[BlockNumber]]
filterSequencesOnUserInput userInput [] = []
filterSequencesOnUserInput userInput (x:xs) = do
    if ((length x) >= userInput)
        then do
            [x] ++ filterSequencesOnUserInput userInput xs
        else do
            filterSequencesOnUserInput userInput xs