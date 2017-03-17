-- | Required for use with Cabal, but perhaps only when scraping large chunks of HTML?
{-# LANGUAGE OverloadedStrings #-}

-- | Importing required packages
-- You need to install Scalpel via 'cabal install scalpel' (potentially different for alternative platforms)
-- You may need to install the other packages listed below, you'll be informed when attempting to run the script.
import Control.Monad
import Data.Maybe
import Text.HTML.Scalpel
import Data.Tuple.Select
import Data.List

-- | Declaring fancy types
-- Not neccessary, but makes interpretation of the script easier.
type BlockNumber = Integer
type BlockDiff = Integer
type QuantityBlocks = Integer
type CPID = String
type FullyFormedURL = String
type Counter = Int
type MinimumSequentialBlocks = Int

-- | Global variables
counterInit = 0
gridcoinstatsURL = "https://gridcoinstats.eu/cpid.php?a=view&id="

-- | The main function is called from the command line
-- The user is promted for a CPID and the quantity of sequentially staked blocks to display (e.g. 3 == [[1,2,3], [4,5,6]] )
main :: IO ()
main = do
 -- | Prompt the user for an CPID
 -- TODO: Check validity of cpid -> min/max length.
 foo <- putStrLn "Input CPID!"
 inputCPID <- getLine
 
 -- | Prompt the user for the quantity of sequentially staked blocks required to be shown.
 -- TODO: Prevent 0 and negatives?
 foo2 <- putStrLn "How many blocks in a row do you want to find?"
 userBlockQuery <- getLine

 -- | inputCPID (user input CPID) is fully formed then passed into Scalpel to retrieve the block numbers of staked blocks.
 -- TODO: Write scrapedCPIDs to disk (inputCPID.txt) and check for future runs, preventing repeatedly scraping gridcoinstats.eu
 -- TODO: Look into porting the Bitcoin Haskell packages to Gridcoin as an alternative to scraping gridcoinstats.eu
 scrapedCPIDs <- mapM (\x -> scrapeURL x scrapeCPIDTable) [(formingURL inputCPID)]

 -- | Cleaning up the output from Scalpel, results in a list of block numbers (e.g. [1,2,3])
 -- BUG FOUND: "*** Exception: Maybe.fromJust: Nothing" -> It's possible that gridcoinstats.eu was updating the page's contents and we scraped a partial page?
 -- TODO: Check if scrapedCPIDs is empty!
 let blockNums = concat(fromJust (head scrapedCPIDs))

 -- | Creates a list of lists which contain sequential blocks (e.g. [[1,2,3], [4,5], [6,7,8]])
 let sequentialBlockList = createLists blockNums

 -- | Creates the output data - filters lists of sequential blocks which have a length smaller than the user input variable 'userBlockQuery'
 let output = filterSequencesOnUserInput (read userBlockQuery) sequentialBlockList

 -- | Output text to the command line
 -- Could potentially be written to disk instead.
 print output

-- | Pass input (inputCPID) from main, prepend the Gridcoinstats.eu URL and return as a fully formed URL.
formingURL :: CPID -> FullyFormedURL
formingURL input = gridcoinstatsURL ++ input

-- | The initially called Scalpel function
-- This returns each <td> element which has a style="text-align: center"
-- This <td> element contains the staked blocks.
-- Note: If gridcoinstats.eu changes, this will need modified.
scrapeCPIDTable :: Scraper String [[BlockNumber]]
scrapeCPIDTable = chroots ("td" @: ["style" @= "text-align: center"]) scrapeCPID

-- | Final section of the Scalpel functional
-- Called from the 'scrapeCPIDTable' function, handed each <td> element.
-- Each <td> element has the hyperlink "a" text extracted (the CPID).
scrapeCPID :: Scraper String [BlockNumber]
scrapeCPID = do
    cpid <- text $ "a"
    return [read cpid]

-- | Called when initialising 'sequentialBlockList' in the main function.
-- We pass in the list of blocks (e.g. [1,2,3]) and return a list of list of sequential blocks (e.g. [[1,2],[3,4,5],[6,7]] ).
-- Automatically skips over blocks in between sequences of blocks.
createLists :: [BlockNumber] -> [[BlockNumber]]
createLists [] = []
createLists (x:xs) = do
    -- | Check if we're passing in an empty list, if so return [].
    if (xs == [])
        then do
            []
        else do
            -- | Check if two blocks are in sequence, if so begin gathering sequence, otherwise recursively call 'createLists' (skipping the block).
            if (((head(xs)) - x) == 1)
              then do
                -- | sequentialChunk: Splits the input list of blocks into two lists within a touple ([list1], [list2])
                -- To establish where to split the list, we call the function 'quantitySequentialBlocks' which takes the full list (x:xs) and a counter initializer (0) as input parameters.
                let sequenctialChunk = splitAt (fromJust(quantitySequentialBlocks (x:xs) counterInit)) xs

                -- | currentChunk and recursiveChunk -> from the previous step we take the touple of lists and assign them to seperate variables.
                let currentChunk = fst sequenctialChunk
                let recursiveChunk = snd sequenctialChunk
                
                -- | We check if the recursiveChunk is empty (passed in the very last block, no further blocks)
                if (recursiveChunk /= [])
                  then do
                    -- | Further blocks to analyse; we create the new list then recursively call 'createLists' with the recursiveChunk.
                    [[x] ++ currentChunk] ++ createLists recursiveChunk
                  else do
                    -- | No more blocks to analyse after this iteration; return the final list of sequential blocks.
                    [[x] ++ currentChunk]
              else do
                -- | Skip blocks which have a distance greater than 1
                createLists xs

-- | Function required by 'sequentialChunk' within the 'CreateLists' function.
-- The function is handed the current full list and an initial counter value (0)
quantitySequentialBlocks :: [BlockNumber] -> Counter -> Maybe Counter
quantitySequentialBlocks (x:xs) counter = do
    -- | Check distance between first two blocks within passed list of blocks
    if (((head(xs)) - x) == 1)
      then do
        -- | If the blocks are sequential, increment the counter then recursively call 'quantitySequentialBlocks' to check if the next blocks in the list are sequential.
        quantitySequentialBlocks xs (counter + 1)
      else do
        -- | If the next block is not sequential, then return the counter value which is where we split the list at.
        -- by using 'return' we encase the counter in a Monad, this monad casing is removed by 'fromJust' in the 'sequentialChunk' initialisation phase.
        return (counter)

-- | At this point we've got a large list of sequential blocks, but who cares about two blocks in a row? We want the user to be able to check for 6+ blocks in a row, etc.
-- We pass in the list of sequential lists, and return a list of sequential lists (minus lists with a smaller length than user input)
filterSequencesOnUserInput :: MinimumSequentialBlocks -> [[BlockNumber]] -> [[BlockNumber]]
filterSequencesOnUserInput userInput [] = []
filterSequencesOnUserInput userInput (x:xs) = do
    -- | Check if the length of the list is equal to or greater than the userInput quantity.
    if ((length x) >= userInput)
        then do
            -- | If equal or greater, we return the list and recursively call the "filterSequencesOnUserInput" to continuously check the rest of the list of lists.
            [x] ++ filterSequencesOnUserInput userInput xs
        else do
            -- | If less than the user input, we skip the list (ommiting from the output) and recursively call the "filterSequencesOnUserInput" function.
            filterSequencesOnUserInput userInput xs