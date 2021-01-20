module Comonad where

import Data.Bits
import Data.Bool
import Data.List (intercalate, elemIndex)
import Data.Maybe (fromJust)

class Functor w => Comonad w where
    (=>>) :: w a -> (w a -> b) -> w b
    coreturn :: w a -> a
    cojoin :: w a -> w (w a)
    x =>> f = fmap f (cojoin x)

-- Zipper-ish universe
data U a = U [a] a [a]

instance Show a => Show (U a) where
    show (U l c r) = intercalate "," [lString, show c, rString]
        where showList = intercalate "," . map show
              lString = "..." ++ showList (reverse $ take 2 l)
              rString = showList (take 2 r) ++ ",..."

instance Functor U where
    fmap f (U l c r) = U (map f l) (f c) (map f r)

instance Comonad U where
    cojoin u = U (tail $ iterate left u) u (tail $ iterate right u)
    coreturn (U _ c _) = c

right :: U a -> U a
right (U ls c (r:rs)) = U (c:ls) r rs

left :: U a -> U a
left (U (l:ls) c rs) = U ls l (c:rs)

(>>>) :: U a -> Int -> U a
u >>> n = iterate right u !! n

(<<<) :: U a -> Int -> U a
u <<< n = iterate left u !! n

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

-- Some example universes
x :: U Int
x = U [-1,-2..] 0 [1..]

y :: U Bool
y = U allFalse True allFalse
