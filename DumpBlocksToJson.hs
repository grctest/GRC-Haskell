#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

-- | Core functionality imports
import Turtle
import Data.Text
import Numeric

main = do
    foo <- putStrLn "Enter starting block number:"
    startingBlock <- getLine

    foo <- putStrLn "Enter the last block number:"
    lastBlock <- getLine
    
    let blockNums = [(read startingBlock) .. (read lastBlock)]

    createBlockDetails blockNums ((read lastBlock :: Float) - (read startingBlock :: Float)) (1.00  :: Float)

createBlockDetails :: [Integer] -> Float -> Float -> IO ExitCode
createBlockDetails (x:xs) quantityBlocks counter = do
    --print "test"
    if (xs /= [])
        then do
            print ((counter/quantityBlocks)*100)
            shell (pack ("gridcoinresearchd getblockbynumber " ++ show x ++ " > echo >> json/blocks.json")) Turtle.empty
            createBlockDetails xs quantityBlocks (counter + 1)
        else do
            shell (pack ("gridcoinresearchd getblockbynumber " ++ show x ++ " > echo >> json/blocks.json")) Turtle.empty