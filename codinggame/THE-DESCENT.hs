-- https://www.codingame.com/training/easy/the-descent
import System.IO
import Control.Monad
import Control.Applicative

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE
    -- The while loop represents the game.
    -- Each iteration represents a turn of the game
    -- where you are given inputs (the heights of the mountains)
    -- and where you have to print an output (the index of the mountain to fire on)
    -- The inputs you are given are automatically updated according to your last actions.
    loop

loop :: IO ()
loop = do

    let mhs = replicateM 8 $ do
        input_line <- getLine
        let mountainh = read input_line :: Int -- represents the height of one mountain.
        return mountainh

    max <- maximum <$> (`zip` [0..]) <$> mhs
    -- The index of the mountain to fire on.
    putStrLn(show $ snd max)
loop
