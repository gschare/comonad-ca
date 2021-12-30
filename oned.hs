module OneD where

import Comonad
import Row

import Data.Bits
import Data.Bool
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- Cellular automata with integer rules as Wolfram Codes
allFalse :: [Bool]
allFalse = False:allFalse

itou :: Int -> U Bool
itou x = U allFalse y (ys ++ allFalse)
    where nbits = floor $ logBase 2 $ fromIntegral x
          (y:ys) = map (testBit x) [nbits,nbits-1..0]

patterns :: [[Bool]]
patterns = sequence $ replicate 3 [False,True]

next :: Int -> U Bool -> Bool
next rule (U (l:_) c (r:_)) = rule `testBit` (fromJust $ [l,c,r] `elemIndex` patterns)

-- This is the coolest function in the module.
-- It makes use of the comonad instance for U in order to (lazily) generate local context for all foci and then map them all with the given function.
-- The given function in this instance of course is the one to take a universe and generate the next state for the focus!
-- Hence we are "flatMap"-ing the universe, by exploding it into infinite miniverses and then collapsing them all again into one new universe.
nextUniverse :: Int -> U Bool -> U Bool
nextUniverse rule u = (=>> next rule) u

-- Do the nextUniverse thing a bunch of times!
nextN :: Int -> U Bool -> Int -> [U Bool]
nextN rule u n = take n $ iterate (nextUniverse rule) u

showUniverse :: Int -> Char -> Char -> U Bool -> [Char]
showUniverse width off on (U l c r) = l' ++ [c'] ++ r'
    where btoa = bool off on
          showSide = map btoa . take (width `div` 2)
          l' = reverse $ showSide l
          r' = showSide r
          c' = btoa c

-- convenience pretty printing function
pp :: Int -> U Bool -> Char -> Char -> Int -> Int -> IO ()
pp rule initial off on width height =
    mapM_ putStrLn $
        map (showUniverse width off on) $
            nextN rule initial height

main :: IO ()
main = do
    -- Sierpenski triangle: rule 90
    pp 90 ((itou 1) >>> 40) ' ' '*' 80 40
    -- Same thing but bigger and centered
    pp 90 (itou 1) ' ' '*' 140 120
