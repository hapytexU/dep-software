{-# LANGUAGE DeriveFoldable, DeriveFunctor #-}

module Dep.Data.Three where

data Three a
  = Leaf a
  | Link (Three a)
  | Split (Three a) (Three a)
  deriving (Eq, Foldable, Functor, Ord, Read, Show)
