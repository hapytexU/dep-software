module Dep.Utils where

toList' :: Foldable f => f a -> [a] -> [a]
toList' = flip (foldr (:))
