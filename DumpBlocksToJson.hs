#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

-- | Core functionality imports
import Turtle
import Data.Text

-- | Main function - called from prelude/terminal.
main = do
    -- | User is prompted for a starting point
    foo <- putStrLn "Enter starting block number:"
    startingBlock <- getLine

    -- | User is prompted for an end point
    -- TODO: Default = latest block (getinfo parsing)
    -- Need to prevent entering a block number greater than the current quantity of blocks!
    -- If this value exceeds current block count, the script will append a warning from the client potentially thousands of times!
    foo <- putStrLn "Enter the last block number:"
    lastBlock <- getLine
    
    -- | Create the list of blocks between the user input inputs
    let blockNums = [(read startingBlock) .. (read lastBlock)]

    -- | Calls the createBlockDetails function, passes in the list of blocks (blockNums), the quantity of blocks and an initial counter.
    createBlockDetails blockNums ((read lastBlock :: Float) - (read startingBlock :: Float)) (1.00  :: Float)

createBlockDetails :: [Integer] -> Float -> Float -> IO ExitCode
createBlockDetails (x:xs) quantityBlocks counter = do
    -- | Check if xs is empty (x is last block to query)
    if (xs /= [])
        then do
            -- | Print the current % completion.
            -- TODO: Replace with a progress bar, or replace the line instead of printing new lines.
            print ((counter/quantityBlocks)*100)

            -- | Using Turtle, run the shell command 'gridcoinresearchd getblockbynumber #' and append the output to the blocks.json file.
            shell (pack ("gridcoinresearchd getblockbynumber " ++ show x ++ " > echo >> json/blocks.json")) Turtle.empty

            -- | Recursively call this function to query the gridcoin client for the next block. Maintain a static quantityBlocks and increment the counter (for % calc).
            createBlockDetails xs quantityBlocks (counter + 1)
        else do
            -- | Since xs is [] (empty), we have reached the final block.
            shell (pack ("gridcoinresearchd getblockbynumber " ++ show x ++ " > echo >> json/blocks.json")) Turtle.empty