{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable, Safe, TemplateHaskellQuotes #-}

module Dep.Algorithm.Levenshtein (
    -- * Present edits to a sequence
    Edit(Add, Rem, Copy, Swap)
    -- * Edit distance score
  , editScore, editScore'
    -- * Determine the most optimal edit
  , levenshtein, levenshtein', reversedLevenshtein, reversedLevenshtein'
    -- * Advanced Levenshtein distances
  , genericReversedLevenshtein, genericReversedLevenshtein'
  ) where

import Control.Arrow(second)
import Control.DeepSeq(NFData, NFData1)

import Data.Binary(Binary(put, get), getWord8, putWord8)
import Data.Data(Data)
import Data.Functor.Classes(Eq1(liftEq), Ord1(liftCompare))
import Data.Hashable(Hashable)
import Data.Hashable.Lifted(Hashable1)

import Dep.Utils(applyExp')

import GHC.Generics(Generic, Generic1)

import Language.Haskell.TH.Syntax(Lift(lift, liftTyped), TExp(TExp))

import Test.QuickCheck(oneof)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary, shrink), Arbitrary1(liftArbitrary), arbitrary1)

data Edit a
  = Add a
  | Rem a
  | Copy a
  | Swap a a
  deriving (Data, Eq, Foldable, Functor, Generic, Generic1, Ord, Read, Show, Traversable)

-- | Determine the standard edit score for the /Levenshtein distance/.
editScore
  :: Edit a  -- ^ The given 'Edit' to convert to a score.
  -> Int  -- ^ The score of the given 'Edit' object.
editScore (Add _) = 1
editScore (Rem _) = 1
editScore (Copy _) = 0
editScore (Swap _ _) = 1

-- | Determine the score for the /Levenshtein distance/ for a 'Foldable' of 'Edit's.
editScore' :: Foldable f
  => f (Edit a)  -- ^ The given 'Foldable' of edits to determine the score from.
  -> Int  -- ^ The edit score given for the given 'Foldable' of 'Edit's.
editScore' = foldr ((+) . editScore) 0

instance Arbitrary1 Edit where
    liftArbitrary arb = go
      where go = oneof [Add <$> arb, Rem <$> arb, Copy <$> arb, Swap <$> arb <*> arb]

instance Arbitrary a => Arbitrary (Edit a) where
    arbitrary = arbitrary1
    shrink (Add x) = Add <$> shrink x
    shrink (Rem x) = Rem <$> shrink x
    shrink (Copy x) = Copy <$> shrink x
    shrink (Swap x y) = Swap <$> shrink x <*> shrink y

instance Binary a => Binary (Edit a) where
    put (Add x) = putWord8 0 >> put x
    put (Rem x) = putWord8 1 >> put x
    put (Copy x) = putWord8 2 >> put x
    put (Swap x y) = putWord8 3 >> put x >> put y
    get = do
        tp <- getWord8
        case tp of
          0 -> Add <$> get
          1 -> Rem <$> get
          2 -> Copy <$> get
          3 -> Swap <$> get <*> get
          _ -> fail ("The numer " ++ show tp ++ " is not a valid Edit item.")


instance Eq1 Edit where
  liftEq cmp = go
    where go (Add l) (Add r) = cmp l r
          go (Rem l) (Rem r) = cmp l r
          go (Copy l) (Copy r) = cmp l r
          go (Swap la lb) (Swap ra rb) = cmp la ra && cmp lb rb
          go _ _ = False

instance Hashable a => Hashable (Edit a)

instance Hashable1 Edit

instance Lift a => Lift (Edit a) where
  liftTyped = fmap TExp . lift
  lift (Add a) = applyExp' 'Add [a]
  lift (Rem a) = applyExp' 'Rem [a]
  lift (Copy a) = applyExp' 'Copy [a]
  lift (Swap a b) = applyExp' 'Swap [a, b]

instance NFData a => NFData (Edit a)

instance NFData1 Edit

instance Ord1 Edit where
  liftCompare cmp = go
    where go (Add a) (Add b) = cmp a b
          go (Add _) _ = LT
          go _ (Add _) = GT
          go (Rem a) (Rem b) = cmp a b
          go (Rem _) _ = LT
          go _ (Rem _) = GT
          go (Copy a) (Copy b) = cmp a b
          go (Copy _) _ = LT
          go _ (Copy _) = GT
          go (Swap la lb) (Swap ra rb) = cmp la ra <> cmp lb rb

levenshtein :: Eq a => [a] -> [a] -> (Int, [Edit a])
levenshtein = levenshtein' (==)

levenshtein' :: (a -> a -> Bool) -> [a] -> [a] -> (Int, [Edit a])
levenshtein' eq xs' ys' = second reverse (reversedLevenshtein' eq xs' ys')

reversedLevenshtein :: Eq a => [a] -> [a] -> (Int, [Edit a])
reversedLevenshtein = reversedLevenshtein' (==)

reversedLevenshtein' :: (a -> a -> Bool) -> [a] -> [a] -> (Int, [Edit a])
reversedLevenshtein' eq = genericReversedLevenshtein' eq c1 c1 (const c1)
  where c1 = const 1

genericReversedLevenshtein :: Eq a => (a -> Int) -> (a -> Int) -> (a -> a -> Int) -> [a] -> [a] -> (Int, [Edit a])
genericReversedLevenshtein = genericReversedLevenshtein' (==)

genericReversedLevenshtein' :: (a -> a -> Bool) -> (a -> Int) -> (a -> Int) -> (a -> a -> Int) -> [a] -> [a] -> (Int, [Edit a])
genericReversedLevenshtein' eq ad rm sw xs' ys' = last (foldl (nextRow ys') row0 xs')
  where
    row0 = scanl (\(w, is) i -> (w+ad i, Add i: is)) (0, []) ys'
    nextCell x (l, le) y (lt, lte) (t, te)
      | eq x y = (lt, Copy x : lte)
      | scs <= scr && lt <= sca = (scs, Swap x y:lte)
      | sca <= scr = (sca, Add y:le)
      | otherwise = (scr, Rem x:te)
      where sca = l + ad y
            scr = t + rm x
            scs = lt + sw x y
    curryNextCell x l = uncurry (uncurry (nextCell x l))
    nextRow ys da@(~((dn, de):ds)) x = scanl (curryNextCell x) (dn+rm x,Rem x:de) (zip (zip ys da) ds)
