-- | Required for use with Scalpel, but perhaps only when scraping large chunks of HTML?
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
 scrapedCPIDs <- scrapeURL (formingURL inputCPID) scrapeCPIDTable
   -- Check if scrapedCPIDs is empty! -> avoids *** Exception: Maybe.fromJust: Nothing (It's possible that gridcoinstats.eu was updating the page's contents and we scraped a partial page?)
 guard $ (case scrapedCPIDs of 
            Just _ -> True
            Nothing -> False)

 -- | Cleaning up the output from Scalpel, results in a list of block numbers (e.g. [1,2,3])
 let blockNums = concat(fromJust scrapedCPIDs)
 
 -- | Creates a list of lists which contain sequential blocks (e.g. [[1,2,3], [4,5], [6,7,8]])
 let sequentialBlockList = groupBy (\ a b -> b-a==1) $ sort blockNums -- createLists blockNums
 print sequentialBlockList
 
 -- | Creates the output data - filters lists of sequential blocks which have a length smaller than the user input variable 'userBlockQuery'
 let output = filter (\ seq -> length seq >= (read userBlockQuery)) sequentialBlockList

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
-- Each <td> element has the hyperlink "a" text extracted (the Block Number).
scrapeCPID :: Scraper String [BlockNumber]
scrapeCPID = do
    cpid <- text $ "a"
    return [read cpid]

