-- kadane.hs
-- Kadane's algorithm in Haskell

module Main where

-- returns the maximum sum of the substring
kadane :: [Int] -> Int
kadane = snd . foldl step (0, minBound)
  where
    step (currentMax, globalMax) x =
        let newCurrent = max x (currentMax + x)
            newGlobal  = max globalMax newCurrent
        in (newCurrent, newGlobal)

-- Example usage
main :: IO ()
main = do
    let arr = [−2,1,−3,4,−1,2,1,−5,4]
    putStrLn "Array:"
    print arr

    let maxSum = kadane arr
    putStrLn "\nMaximum subarray sum:"
    print maxSum
