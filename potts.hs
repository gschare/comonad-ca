import Data.Bool (bool)
import Data.Bits
import Data.Function (on)
import Data.List (groupBy, elemIndex)
import Data.Maybe (fromMaybe)
import System.Random hiding (next)

allFalse :: [Bool]
allFalse = False:allFalse

patterns :: [(Bool,Bool,Bool)]
patterns = map (\x -> (x `testBit` 2, x `testBit` 1, x `testBit` 0)) ([7,6..0] :: [Int])

itob :: Int -> Bool
itob = bool False True . (==1)

main :: IO ()
main = do
  g <- getStdGen
  let rule = 90
  let width = 160
  let height = 160
  let initial = take width $ randoms g :: [Bool]
  pp rule (2 ^ (width `div` 2)) width height

pp :: Int -> Int -> Int -> Int -> IO ()
pp rule initial width height = mapM_ putStrLn $ map showLine $ take height $ iterate (nextLine rule) $ map (testBit initial) [width,width-1..0]

expand :: Int -> [Bool] -> [Bool]
expand n known = (take n allFalse) ++ known ++ (take n allFalse)

next :: Int -> (Bool,Bool,Bool) -> Bool
next 90 (l,_,r) = l /= r
next rule t = rule `testBit` fromMaybe 0 (t `elemIndex` patterns)

nextLine :: Int -> [Bool] -> [Bool]
nextLine rule bs =
  let bs' = expand 1 bs
  in map (next rule) $ zip3 bs' (tail bs') (tail $ tail bs')

showCell :: Bool -> Char
showCell True  = '*'
showCell False = ' '

showLine :: [Bool] -> String
showLine = map showCell

sierpenski :: IO ()
sierpenski =
    mapM_ putStrLn $
        map (\xs -> map (\(a,b) -> if not a then if b then '/' else ' ' else '\\') $ zip xs $ tail xs) $
            take 100 $ iterate (nextLine 90) $
                replicate 79 False ++ [True] ++ replicate 79 False
