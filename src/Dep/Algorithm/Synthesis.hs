{-# LANGUAGE BangPatterns, Safe #-}

{-|
Module      : Dep.Algorithm.Synthesis
Description : A module to convert a 'Three' of 'ThreeValue's into a sum-of-products of a product-of-sums.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module defines functions to generate a /sum-of-products/ or a /product-of-sums/ with the given
function specified by a 'Three'.
-}

module Dep.Algorithm.Synthesis (
    -- * Synthesize a 'Three'
    synthesis
    -- * Create an upper and lowerbound Three
  , upperbound, lowerbound
    -- * Extract products and sums
  , extractProduct, extractSum
    -- * Processing a 'Three'
  , wipeout
    -- * Check minimizations
  , validMinimize
  ) where

import Control.Applicative((<|>))

import Dep.Core(Walkable(allStep, allWalkValues), NonDeterministicWalkable(allNstep))
import Dep.Data.Product(Product', SumOfProducts')
import Dep.Data.Sum(Sum', ProductOfSums')
import Dep.Data.Three(Three(Leaf, Link, Split), applyTo, depth, simplify)
import Dep.Data.ThreeValue(ThreeValue(DontCare, Zero, One), ThreeValues, toLower, toUpper)

type WeightedItem = (Int, ThreeValues)
type WeightedProduct = (Int, Product')
type WeightedSum = (Int, Sum')

_minWeightedItems :: (Int, a) -> (Int, a) -> (Int, a)
_minWeightedItems x@(xw, _) y@(yw, _)
  | xw <= yw = x
  | otherwise = y

_incStep :: ThreeValue -> Int -> Int
_incStep DontCare = id
_incStep _ = (1+)


-- | Create a /simplified/ 'Three' where the 'DontCare' and 'One' map to 'True';
-- and 'Zero' maps to 'False'.
upperbound
  :: Three ThreeValue -- ^ The given 'Three' of 'ThreeValue's where we calculate the /upperbound/ from.
  -> Three Bool -- ^ The corresponding /upperbound/.
upperbound = simplify . fmap toUpper

-- | Create a /simplified/ 'Three' where the 'DontCare' and 'Zero' map to 'False';
-- and 'One' maps to 'True'.
lowerbound
  :: Three ThreeValue -- ^ The given 'Three' of 'ThreeValue's where we calculate the /lowerbound/ from.
  -> Three Bool -- ^ The corresponding /lowerbound/.
lowerbound = simplify . fmap toLower

_pushVal :: (Int -> Int) -> ThreeValue -> (Int, Product') -> (Int, Product')
_pushVal f x = go
    where go ~(!n, xs) = (f n, x:xs)

_pushVal' :: ThreeValue -> (Int, Product') -> (Int, Product')
_pushVal' = _pushVal (1+)

_pushVal'' :: (Int, Product') -> (Int, Product')
_pushVal'' = _pushVal id DontCare

_pushWithVal :: ThreeValue -> (Int, Product') -> (Int, Product')
_pushWithVal DontCare = _pushVal''
_pushWithVal x = _pushVal' x

extractItem
  :: ThreeValue  -- ^ The given 'ThreeValue' we are looking for. Typically this will be 'One' for a sum-of-products, and 'Zero' for a product of sums.
  -> Int  -- Y The maximum depth of the 'Three'.
  -> Three Bool -- ^ A 'Three' of 'Bool's that specifies if 'One' can be assigned to it (so either 'One' or 'DontCare').
  -> Three ThreeValue -- ^ A 'Three' of 'ThreeValue's in which we search for the given element.
  -> Maybe WeightedItem  -- ^ A path to a 'Leaf; with the given 'ThreeValue' together with the number of 'Zero's and 'One's in that path. If no path is found 'Nothing' is returned.
extractItem x' k _ = go k
  where go !n (Leaf x)
          | x == x' = Just (0, replicate n DontCare)
          | otherwise = Nothing
        go n (Link l) = _pushVal'' <$> go (n-1) l
        go n ~(Split la lb) = fmap (_pushVal' Zero) (go (n-1) la) <|> fmap (_pushVal' One) (go (n-1) lb)


-- | Obtain a 'Product'' together with the number of inputs for the AND gate if the 'Three'
-- contains at least one 'One'.
extractProduct
  :: Int  -- | The maximum depth of the 'Three'.
  -> Three Bool -- | A 'Three' of 'Bool's that specifies if 'One' can be assigned to it (so either 'One' or 'DontCare').
  -> Three ThreeValue -- | The 'Three' of 'ThreeValue's where we try to search for an item.
  -> Maybe WeightedProduct -- A 2-tuple that contains the path to the leaf and the number of 'Zero's and 'One's in the path that measure the "weight" of the AND gate of the product.
extractProduct = extractItem One

-- | Obtain a 'Sum'' together with the number of inputs for the OR gates if the 'Three'
-- contains at least one 'Zero'.
extractSum
  :: Int  -- | The maximum depth of the 'Three'.
  -> Three Bool -- | A 'Three' of 'Bool's that specifies if 'Zero' can be assigned to it (so either 'Zero' or 'DontCare').
  -> Three ThreeValue -- | The 'Three' of 'ThreeValue's where we try to search for an item.
  -> Maybe WeightedSum -- A 2-tuple that contains the path to the leaf and the number of 'Zero's and 'One's in the path that measure the "weight" of the OR gate of the product.
extractSum = extractItem Zero


{- specializeProduct :: ThreeValue -> Product' -> (Int, Product')
specializeProduct x = foldr f (0, [])
  where f DontCare ~(!i, xs) = (i+1, x:xs)
        f tv ~(!i, xs) = (i, x:xs) -}

-- | Convert the items that are accessed by the 'Product''
-- to a 'DontCare', and simplify the 'Three'. After wiping
-- out values, the 'Three' is simplified.
wipeout
  :: Product' -- ^ The product that specifies the path of the element(s) to set to 'DontCare'.
  -> Three ThreeValue  -- ^ The original 'Three' of 'ThreeValue's where we want to convert parts to 'DontCare'.
  -> Three ThreeValue  -- ^ The resulting 'Three' of 'ThreeValue's where items that match the path are wiped out.
wipeout path = simplify . wipeout' path

-- | Convert the items that are accessed by the 'Product''
-- to a 'DontCare', and simplify the 'Three'.
wipeout'
  :: Product' -- ^ The product that specifies the path of the element(s) to set to 'DontCare'.
  -> Three ThreeValue  -- ^ The original 'Three' of 'ThreeValue's where we want to convert parts to 'DontCare'.
  -> Three ThreeValue  -- ^ The resulting 'Three' of 'ThreeValue's where items that match the path are wiped out.
wipeout' = applyTo (const DontCare)

_checkMinimize :: [Three Bool] -> Bool -> [Bool] -> Bool
_checkMinimize sts stp stps = and (allWalkValues (allStep sts stp) stps)

-- | Check if we follow the current non-determinisic path, we only
-- find 'True' values wrapped in the nodes.
validMinimize
  :: [Three Bool] -- ^ The given 'Three' of 'Bool's that we search for.
  -> [Bool]  -- ^ The given path we need to follow.
  -> Bool -- ^ A 'Bool' that indicates that the nodes where we end only wraps 'True' values.
validMinimize tbs stps = and (allWalkValues tbs stps)

_allTrue :: (Foldable f, Functor f, Foldable g) => f (g Bool) -> Bool
_allTrue = and . fmap and

{-
minimizeMin' :: ([Int] -> BitThSeq) -> [Int] -> [Int] -> Int -> ([Int],Int)
minimizeMin' l h [] w | validMin l h = (h,w)
                      | otherwise = (h,w+9999999)
minimizeMin' l h (x:xs) w | not $ validMin l (h++(x:xs)) = (h,w+9999999)
                          | wa <= wb = (ha,wa)
                          | otherwise = (hb,wb)
                          where (ha,wa) = minimizeMin' l h xs (w-1)
                                (hb,wb) = minimizeMin' l (h++[x]) xs w

-}

minimizeProduct :: Int -> Int -> Product' -> Three Bool -> Product'
minimizeProduct dpth mw prd tr = maybe prd snd (minimizeProduct' dpth mw (expandProduct prd) [tr])

-- first parameter: maximum cost (if <= 0, STOP)
-- second parameter: the current path we are tracing (moves to go)
-- third parameter: the nodes we are currently located (per move, update the list)
-- return type: a possible product that is weighed
-- *second parameter: depth to go (if depth <= cost, STOP)

minimizeProduct' :: Int -> [Bool] -> [Three Bool] -> Maybe (Int, Product')
minimizeProduct' cst | cst <= 0 = Nothing
minimizeProduct' cst = go
  where go (False:xs) ts | _allTrue (allstep ts True) = minimizeProduct cst xs (allNstep ts DontCare)
          where nextStep = allNstep ts DontCare -- potential to be nothing
  {-
minimizeProduct' _ _ [] ts
  | _allTrue ts = Just (0, [])
  | otherwise = Nothing
minimizeProduct' cst depth _ _
  | cst >= depth = Nothing
minimizeProduct' cst dpt ~(x:xs) ts
  | False <- x = minimizeProduct' cst (dpt-1) (allNstep ts DontCare) xs
  | True <- x = _pushVal'' <$> minimizeProduct' cst (dpt-1) (allNstep ts DontCare) xs
  | otherwise = Nothing  -- one
  where toNothing = minimizeProduct' (cst-1) (dpt-1) -- the cost remains the same
-}
-- expand :: ThreeValue -> [Three Bool] -> Maybe (Int, Product')


  {-
minimizeMin' :: ([Int] -> BitThSeq) -> [Int] -> [Int] -> Int -> ([Int],Int)
minimizeMin' l h [] w | validMin l h = (h,w)
                      | otherwise = (h,w+9999999)
minimizeMin' l h (x:xs) w | not $ validMin l (h++(x:xs)) = (h,w+9999999)
                          | wa <= wb = (ha,wa)
                          | otherwise = (hb,wb)
                          where (ha,wa) = minimizeMin' l h xs (w-1)
                                (hb,wb) = minimizeMin' l (h++[x]) xs w

validMin :: ([Int] -> BitThSeq) -> [Int] -> Bool
validMin f = all (canAssign T) . f

minimizeMin' :: [Three Bool] -> [Int] -> [Int] -> Int -> (Int, [Int])
minimizeMin' l h [] w | validMin l h = (h,w)
                      | otherwise = (h,w+9999999)
minimizeMin' l h (x:xs) w | not $ validMin l (h++(x:xs)) = (h,w+9999999)
                          | wa <= wb = (ha,wa)
                          | otherwise = (hb,wb)
                          where (ha,wa) = minimizeMin' l h xs (w-1)
                                (hb,wb) = minimizeMin' l (h++[x]) xs w
-}

expandProduct :: Product' -> [Bool]
expandProduct = map f
  where f One = True
        f _ = False

-- | Create a sum-of-products for the given function of 'ThreeValue'.
synthesis :: Three ThreeValue -> SumOfProducts'
synthesis th = _synthesis _simp
  where _upper = upperbound _simp
        n = depth _simp
        _simp = simplify th
        _takeProduct = extractProduct n _upper
        _synthesis thr
          | Just (~(k, j)) <- _takeProduct thr = let j' = minimizeProduct k n j _upper in j' : _synthesis (wipeout j' thr)
          | otherwise = []

{-- synthetize
synthetize :: CombTable -> [CombElem]
synthetize ct = map (synthetizeFun . extractFi ct) [1..(ysize ct)]

specialize :: Int -> Int -> [Int] -> [Int]
specialize i n [] = map negate [i..n]
specialize i n (x:xs) | i > n = []
                      | i == ax = x : sin xs
                      | otherwise = -i : sin (x:xs)
                      where ax = abs x
                            sin = specialize (i+1) n

synthetizeFun :: CombFunc -> CombElem
synthetizeFun (CFS n tr _) = sop $ synth tr
    where synth t | Just _ <- smt = so : synth tb
                  | otherwise = []
                  where smt = scrapeMin 1 t
                        sm = fromJust smt
                        lb = length sm
                        ss = specialize 1 n sm
                        (so,_) = maximizeMin t ss
                        tb = blankout so t
synthetizeFun l = synthetizeFun $ cacheCF l

scrapeMin :: Int -> Three BitTh -> Maybe [Int]
scrapeMin _ (ThLeaf T) = Just []
scrapeMin _ (ThLeaf _) = Nothing
scrapeMin n (ThDirect d) = scrapeMin (n+1) d
scrapeMin n (ThNode la lb) | Just ra <- smn1 la = Just (-n:ra)
                           | otherwise = smn1 lb >>= Just . (n:)
                           where smn1 = scrapeMin (n+1)

blankout :: [Int] -> Three BitTh -> Three BitTh
blankout a = reduce . rabl 1 a

rabl :: Int -> [Int] -> Three BitTh -> Three BitTh
rabl i xl@(x:xs) (ThNode tl tr) | i /= xa = ThNode (rabli xl tl) $ rabli xl tr
                                | x < 0 = ThNode (rabli xs tl) tr
                                | otherwise = ThNode tl $ rabli xs tr
    where xa = abs x
          rabli = rabl (i+1)
rabl i xl@(x:xs) (ThDirect d) | i /= xa = ThDirect $ rabli xl d
                              | x < 0 = ThNode (rax d) d
                              | otherwise = ThNode d $ rax d

    where xa = abs x
          rabli = rabl (i+1)
          rax = rabli xs
rabl i xl@(x:xs) vl@(ThLeaf v) | i /= xa = ThDirect $ rabli xl vl
                               | x < 0 = ThNode (rabli xs vl) vl
                               | otherwise = ThNode vl $ rabli xs vl
    where xa = abs x
          rabli = rabl (i+1)
rabl _ [] _ = ThLeaf D

maximizeMin :: Three BitTh -> [Int] -> ([Int],Int)
maximizeMin t x = minimizeMin' (pathSeq t) [] x (length x)

-}
{-
minimizeProduct' :: Product' -> [Three Bool] -> (Int, Product')
minimizeProduct' [] _ = (0, [])
minimizeProduct' (DontCare:xs) ts = (DontCare:) <$> minimizeProduct' xs (allnstep ts DontCare)
minimizeProduct' (x:xs) ts = (x:) <$> minimizeProduct' xs (allnstep ts DontCare)
--  | True <- c
-}
