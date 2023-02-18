module Utils.Internal where

import Control.Lens
import Data.Maybe (mapMaybe)

uniqBy :: (a -> a -> Bool) -> [a] -> [a]
uniqBy _ [] = []
uniqBy f (x : xs) = x : uniqBy f (filter (not . f x) xs)

compareBy :: Eq b => (a -> b) -> a -> a -> Bool
compareBy f x y = f x == f y

evalPrism :: Traversal s t a b -> Prism' a b -> [s] -> [t]
evalPrism theTrav thePrism =
    mapMaybe $ theTrav (preview thePrism)