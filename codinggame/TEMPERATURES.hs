-- https://www.codingame.com/training/easy/temperatures

import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.

    input_line <- getLine
    let n = read input_line :: Int -- the number of temperatures to analyse
    temps <- getLine
    -- the n temperatures expressed as integers ranging from -273 to 5526
    let nums = map read (words temps) :: [Int]

    -- Write answer to stdout
    let result = if (length nums) == 0 then 0 else foldl (\x y -> if ((abs y) < (abs x)) || (abs y == abs x && x < 0)  then y else x ) 5526 nums
    putStrLn $ show result
    return ()
