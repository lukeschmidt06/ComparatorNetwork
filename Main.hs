module Main where

import Data.Char
import Data.List
import System.Environment

-- Creating type for Network so as not to repeat [(Int, Int)]
type Network = [(Int, Int)]


-- formates the network into a string then uses writeFile to write the string into the desired file
writeNetwork :: FilePath -> Network -> IO ()
writeNetwork filename network =
    writeFile filename (formatNetwork network)

-- Read a comparator network from a file
readNetwork :: FilePath -> IO Network
readNetwork filename = do
    fileInfo <- readFile filename
    case reads fileInfo :: [(Network, String)] of
        [(network, "")] -> return network

-- Creates the desried format of the txt file based on the tuple created in readNetwork
formatNetwork :: Network -> String
formatNetwork = unlines . map formatPair
  where
    formatPair (x, y) = show x ++ " -- " ++ show y

-- takes in a network and applies the reverse function on it
reverseNetwork :: Network -> Network
reverseNetwork = reverse


applyNetwork :: Network -> [Int] -> [Int]
applyNetwork network sequence = foldl applyComparator sequence network
  where
    applyComparator :: [Int] -> (Int, Int) -> [Int]
    applyComparator seq (x, y)
      | seq !! (x - 1) > seq !! (y - 1) = swap x y seq
      | otherwise = seq

    swap :: Int -> Int -> [Int] -> [Int]
    swap x y seq = map (\i -> if i == x then seq !! (y - 1) else if i == y then seq !! (x - 1) else seq !! (i - 1)) [1..length seq]

toParallelForm :: Network -> [[(Int, Int)]]
toParallelForm network = groupBy sameWires $ sortBy compareWires network
  where
    compareWires (x1, y1) (x2, y2) = compare (x1, y1) (x2, y2)
    sameWires (x1, _) (x2, _) = x1 == x2
    
-- Format a single parallel comparison pair
formatPair :: (Int, Int) -> String
formatPair (x, y) = show x ++ " -- " ++ show y

-- Format a list of parallel comparison pairs
formatParallel :: [(Int, Int)] -> String
formatParallel = intercalate " , " . map formatPair

-- Format and write parallel form to a file
writeParallel :: FilePath -> [[(Int, Int)]] -> IO ()
writeParallel filename parallelForm = writeFile filename (unlines formattedLines)
  where
    formattedLines = map formatGroup parallelForm
    formatGroup group = intercalate " , " (map formatPair group)
    formatPair (x, y) = show x ++ " -- " ++ show y

-- Main function - has two arguments prepared Read and Reverse that each implement the above functions
main :: IO () 
main = do
    args <- getArgs
    case args of
        ["Read", filename] -> do
            network <- readNetwork filename
            writeNetwork "network.txt" network
        ["Reverse", filename] -> do
            network <- readNetwork filename
            let reversedNetwork = reverseNetwork network
            writeNetwork "reverse.txt" reversedNetwork
        ["Run", filename, sequenceStr] -> do
            network <- readNetwork filename
            let sequence = read sequenceStr :: [Int]
                result = applyNetwork network sequence
            putStrLn $ show result
        ["Parallel", filename] -> do
            network <- readNetwork filename
            let parallelForm = toParallelForm network
            writeParallel "parallel.txt" parallelForm