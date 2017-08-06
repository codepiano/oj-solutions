-- https://www.codingame.com/training/easy/power-of-thor-episode-1
import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    -- ---
    -- Hint: You can use the debug stream to print initialTX and initialTY, if Thor seems not follow your orders.

    input_line <- getLine
    let input = words input_line
    let lightx = read (input!!0) :: Int -- the X position of the light of power
    let lighty = read (input!!1) :: Int -- the Y position of the light of power
    let initialtx = read (input!!2) :: Int -- Thor's starting X position
    let initialty = read (input!!3) :: Int -- Thor's starting Y position
    loop (lightx, lighty, initialtx, initialty)

loop :: (Int, Int, Int, Int) -> IO ()
loop (a, b, c, d) = do
    input_line <- getLine
    let remainingturns = read input_line :: Int -- The remaining amount of turns Thor can move. Do not remove this line.

    -- hPutStrLn stderr "Debug messages..."

    -- A single line providing the move to be made: N NE E SE S SW W or NW
    let relativeX = c - a
    let relativeY = d - b
    let ty = ns relativeY
    let tx = ew relativeX
    putStr $ snd ty
    putStrLn $ snd tx

    loop (a, b, c+(fst tx), d+(fst ty))

ns :: Int -> (Int, String)
ns ry
    | ry > 0 = (-1, "N")
    | ry < 0 = (1, "S")
    | otherwise = (0, "")

ew :: Int -> (Int, String)
ew rx
    | rx > 0 = (-1, "W")
    | rx < 0 = (1, "E")
    | otherwise = (0, "")
