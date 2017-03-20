{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

import Data.Char (toLower)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.List.Split as DLS
import Data.Maybe
import GHC.Generics

-- | Declaring fancy types
-- Not neccessary, but makes interpretation of the script easier.
type BlockNumber = String
type CPID = String
type Address = String
type CPIDandAddress = String
type Counter = Int
type MinimumSequentialBlocks = Int

-- | Global variables
counterInit = 0

-- | Configuring Aeson to handle the blocks.json fields
-- TXID has been missed skipped to simplify the script.
data GetBlockByNumber = GetBlockByNumber { height :: Integer
                                         , cpid :: String -- The json file at this point has uppercase variables, may be an issue?
                                         , grcaddress :: String
                                         } deriving (Show, Generic, Eq)

--data GetBlockByNumber = GetBlockByNumber { hash :: String
--                                         , confirmations :: Integer
--                                         , size :: Integer
--                                         , height :: Integer
--                                         , version :: Integer
--                                         , merkleroot :: String
--                                         , mint :: Float
--                                         , time :: Integer
--                                         , nonce :: Integer
--                                         , bits :: String
--                                         , difficulty :: Float
--                                         , blocktrust :: String
--                                         , chaintrust :: String
--                                         , previousblockhash :: String
--                                         , nextblockhash :: String
--                                         , flags :: String
--                                         , proofhash :: String
--                                         , entropybit :: Integer
--                                         , modifier :: String
--                                         , modifierchecksum :: String
--                                         , cpid :: String -- The json file at this point has uppercase variables, may be an issue?
--                                         , magnitude :: Float
--                                         , lastpaymenttime :: String
--                                         , researchsubsidy :: Float
--                                         , researchage :: Float
--                                         , researchmagnitudeunit :: Float
--                                         , researchaveragemagnitude :: Float
--                                         , lastporblockhash :: String
--                                         , interest :: Float
--                                         , grcaddress :: String
--                                         , clientversion :: String
--                                         , cpidvalid :: Bool
--                                         , neuralhash :: String
--                                         , issuperblock :: Integer
--                                         , iscontract :: Integer
--                                         } deriving (Show, Generic, Eq)

-- | This configures Aeson to handle the json fields properly (json has uppercase keys, this changes keys to lower case.)
instance FromJSON GetBlockByNumber where
  parseJSON = genericParseJSON opts . jsonLower
    where
      opts = defaultOptions { fieldLabelModifier = map toLower }

-- | Turn all keys in a JSON object to lowercase.
jsonLower :: Value -> Value
jsonLower (Object o) = Object . HM.fromList . map lowerPair . HM.toList $ o
  where lowerPair (key, val) = (T.toLower key, val)
jsonLower x = x

-- | Specify the filename of the JSON file we want to import
-- Could make this an user input later
jsonFile :: Prelude.FilePath
jsonFile = "json/blocksMin.json"

-- | Loading the specified JSON file into memory
-- Bear in mind that the fully dumped blockchain is approx 1.6GB!
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

-- | Called first against the loaded JSON fields.
-- For each record in the JSON File we create a tuple and add it to a list.
-- To simplify the script we combine CPID and GRCAddress in the one value (simplifying touple logic).
jsonToTupleList :: [GetBlockByNumber] -> [(BlockNumber, CPIDandAddress)]
jsonToTupleList (x:xs) = do
    if (xs == [])
        then do
            -- cpid, grcaddress, cpidv2, height
            [(show (height x), ((cpid x) ++ " " ++ (grcaddress x)))]
        else do
            [(show (height x), ((cpid x) ++ " " ++ (grcaddress x)))] ++ jsonToTupleList xs

-- | Used to output the list of sequential blocks to terminal
-- May be wise to dump to disk instead of outputting to terminal.
loopPrint :: [(BlockNumber, CPIDandAddress)] -> IO ()
loopPrint (x:xs) = do
    if (xs == [])
        then do
            print x
        else do
            print x
            loopPrint xs

-- | Called when initialising 'sequentialBlockList' in the main function.
-- We pass in the list of blocks (e.g. [(1,"a b"),(2, "b c"), (3, "d e")]) and return a list of list of sequential blocks (e.g. [[(1,"a b")],[(2, "b c"), (3, "d e")]] ).
-- Automatically skips over blocks in between sequences of blocks.
createLists :: [(BlockNumber, CPIDandAddress)] -> Counter -> [[(BlockNumber, CPIDandAddress)]]
createLists [] counter = []
createLists (x:xs) counter = do
    -- | Check if we're passing in an empty list, if so return [].
    if (xs == [])
        then do
            []
        else do

            -- | We need to split the x and first xs touple's CPIDandAddress into two values for comparison
            -- This is repeating code. TODO: Make a function to return these values.
            let cpid = head (DLS.splitOn " " (snd x))
            let cpidXS = head (DLS.splitOn " " (snd (head xs)))    
            let address = head (tail (DLS.splitOn " " (snd x)))
            let addressXS = head (tail (DLS.splitOn " " (snd (head xs))))

            if (cpid == "INVESTOR")
                then do
                    -- | User isn't participating in DPOR, will compare addresses used to stake.
                    -- This won't be effective as there's a 16hr delay for staking with the same address.
                    -- Will need to build a list of associated addresses in order for this to be useful.
                    if (address == addressXS)
                        then do
                            createListLogic (x:xs) counter
                        else do
                        -- | Skip blocks which have a distance greater than 1
                        createLists xs (counter + 1)
                else do
                    -- | User isn't an investor then we will compare CPIDs between blocks
                    if (cpid == cpidXS)
                        then do
                            createListLogic (x:xs) counter
                        else do
                        -- | Skip blocks which have a distance greater than 1
                        createLists xs (counter + 1)

createListLogic :: [(BlockNumber, CPIDandAddress)] -> Counter -> [[(BlockNumber, CPIDandAddress)]]
createListLogic [] counter = []
createListLogic (x:xs) counter = do
    -- | sequentialChunk: Splits the input list of blocks into two lists within a touple ([list1], [list2])
    -- To establish where to split the list, we call the function 'quantitySequentialBlocks' which takes the full list (x:xs) and a counter initializer (0) as input parameters.
    let sequenctialChunk = Prelude.splitAt (fromJust(quantitySequentialBlocks (x:xs) counterInit)) xs

    -- | currentChunk and recursiveChunk -> from the previous step we take the touple of lists and assign them to seperate variables.
    let currentChunk = fst sequenctialChunk
    let recursiveChunk = snd sequenctialChunk

    -- | We check if the recursiveChunk is empty (passed in the very last block, no further blocks)
    if (recursiveChunk /= [])
      then do
        -- | Further blocks to analyse; we create the new list then recursively call 'createLists' with the recursiveChunk.
        [[x] ++ currentChunk] ++ createLists recursiveChunk (counter + 1)
      else do
        -- | No more blocks to analyse after this iteration; return the final list of sequential blocks.
        [[x] ++ currentChunk] 

-- | Function required by 'sequentialChunk' within the 'CreateLists' function.
-- The function is handed the current full list and an initial counter value (0)
quantitySequentialBlocks :: [(BlockNumber, CPIDandAddress)] -> Counter -> Maybe Counter
quantitySequentialBlocks (x:xs) counter = do
    
    -- | We need to split the x and first xs touple's CPIDandAddress into two values for comparison
    let cpid = head (DLS.splitOn " " (snd x))
    let cpidXS = head (DLS.splitOn " " (snd (head xs)))    
    let address = head (tail (DLS.splitOn " " (snd x)))
    let addressXS = head (tail (DLS.splitOn " " (snd (head xs))))

    -- | Check if the block was staked by an investor
    if (cpid == "INVESTOR")
        then do
            -- | If sequential investor blocks, compare addresses.
            -- This is flawed and will not find sequentially staked investor blocks.
            -- Flaw reason: Addresses have a 16hr cooldown.
            -- Potential solution: Build list of associated addresses (would require preprocessing)
            if (address == addressXS)
                then do
                -- | If the blocks are sequential, increment the counter then recursively call 'quantitySequentialBlocks' to check if the next blocks in the list are sequential.
                quantitySequentialBlocks xs (counter + 1)
                else do
                -- | If the next block is not sequential, then return the counter value which is where we split the list at.
                -- by using 'return' we encase the counter in a Monad, this monad casing is removed by 'fromJust' in the 'sequentialChunk' initialisation phase.
                return (counter)
        else do
            -- | If there is a CPID present (DPOR participant), compare CPIDs between blocks.
            -- This is effective, as CPID does not change nor cooldown (unlike investor staking address)
            if (cpid == cpidXS)
                then do
                -- | If the blocks are sequential, increment the counter then recursively call 'quantitySequentialBlocks' to check if the next blocks in the list are sequential.
                quantitySequentialBlocks xs (counter + 1)
                else do
                -- | If the next block is not sequential, then return the counter value which is where we split the list at.
                -- by using 'return' we encase the counter in a Monad, this monad casing is removed by 'fromJust' in the 'sequentialChunk' initialisation phase.
                return (counter)

-- | At this point we've got a large list of sequential blocks, but who cares about two blocks in a row? We want the user to be able to check for 6+ blocks in a row, etc.
-- We pass in the list of sequential lists, and return a list of sequential lists (minus lists with a smaller length than user input)
filterSequencesOnUserInput :: MinimumSequentialBlocks -> [[(BlockNumber, CPIDandAddress)]] -> [[(BlockNumber, CPIDandAddress)]]
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


-- | Main function that is called from prelude!
-- There is currently no user input other than preparing the blocks.json file within the json folder.
-- TODO: Reintroduce 'filterSequencesOnUserInput', enabling users to focus on a specific lengths of sequentially staked blocks.
main :: IO ()
main = do
    -- | Prompt the user for the quantity of sequentially staked blocks required to be shown.
    -- TODO: Prevent 0 and negatives?
    foo2 <- putStrLn "How many blocks in a row do you want to find?"
    userBlockQuery <- getLine
    d <- eitherDecode <$> getJSON :: IO (Either String [GetBlockByNumber])
    case d of
        -- | If there is an error, print the error and quit.
        Left err -> putStrLn err
        -- | If the JSON was read correctly, begin analysis of dumped blocks & print a list of sequential block lists. 
        Right ps -> print (filterSequencesOnUserInput (read userBlockQuery) (createLists (jsonToTupleList ps) 0))