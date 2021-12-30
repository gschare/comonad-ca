module Row where

import Comonad
import Data.List (intercalate)

-- One-dimensional zipper universe
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
