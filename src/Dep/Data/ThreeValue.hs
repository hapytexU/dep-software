{-# LANGUAGE Safe #-}

{-|
Module      : Dep.Data.ThreeValue
Description : A module to define three-value logic.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines three-value logic, for example used in thruth tables, Karnaugh cards, etc.
-}

module Dep.Data.ThreeValue (
    -- * Define three-value logic
    ThreeValue(DontCare, Zero, One)
    -- * Catamorphisms
  , threeValue, toMaybeBool, toChar
    -- * Convert to a 'ThreeValue'
  , fromBool, fromMaybeBool
    -- * Operators on 'ThreeValue'
  , opposite
    -- * Type aliasses
  , ThreeValues
  ) where

import Data.Bool(bool)
import Data.Binary(Binary(put, get), getWord8, putWord8)

import Dep.Core(Mergeable(merge))

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

-- | A data type that is used if a value can present three logical values: /don't care/ (or don't know);
-- /zero/; and /one/.
data ThreeValue
  = Zero  -- ^ The value is /zero/ or /false/.
  | One  -- ^ The value is /one/ or /true/.
  | DontCare  -- ^ We do not care or do not know the value.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

opposite :: ThreeValue -> ThreeValue
opposite Zero = One
opposite One = Zero
opposite x = x

instance Semigroup ThreeValue where
  (<>) DontCare = id
  (<>) x = const x

instance Monoid ThreeValue where
  mempty = DontCare

instance Arbitrary ThreeValue where
  arbitrary = arbitraryBoundedEnum

instance Mergeable ThreeValue where
  merge DontCare x = Just x
  merge x DontCare = Just x
  merge x y
    | x == y = Just x
    | otherwise = Nothing

-- | Convert the given 'ThreeValue' object to the corresponding value.
-- This acts as a /catamorphism/ for the 'ThreeValue' type.
threeValue
  :: a  -- ^ The value for 'DontCare'.
  -> a  -- ^ The value for 'Zero'.
  -> a  -- ^ The value for 'One'.
  -> ThreeValue  -- ^ The value to convert to one of the given values.
  -> a  -- ^ The corresponding value.
threeValue d z o = go
  where go DontCare = d
        go Zero = z
        go ~One = o

-- | Convert 'True' and 'False' to 'Zero' and 'One' respectively.
fromBool :: Bool -> ThreeValue
fromBool = bool Zero One

-- | Convert a 'Maybe' 'Bool' to a 'ThreeValue', where 'Nothing' is
-- mapped to 'DontCare', and 'Just' 'True' and 'Just' 'False' to 'One'
-- and 'Zero'.
fromMaybeBool :: Maybe Bool -> ThreeValue
fromMaybeBool = maybe DontCare fromBool

-- | Convert the given 'ThreeValue' to a 'Maybe' 'Bool' object.
-- where 'DontCare' is mapped to 'Nothing' and 'Zero' and 'One' to
-- 'True' and 'False' wrapped in a 'Just' accordingly.
toMaybeBool
  :: ThreeValue  -- ^ The given 'ThreeValue' to convert.
  -> Maybe Bool  -- ^ The corresponding 'Bool' wrapped in a 'Maybe'.
toMaybeBool = threeValue Nothing (Just False) (Just True)

-- | Convert the given 'ThreeValue' to a 'Char' that presents the given
-- value. 'DontCare' is mapped to @-@; 'Zero' to @0@; and 'One' to @1@.
toChar
  :: ThreeValue  -- ^ The given 'ThreeValue' object to map.
  -> Char  -- ^ The corresponding 'Char' that presents the 'ThreeValue'.
toChar = threeValue '-' '0' '1'

-- | A type alias for a list of 'ThreeValue' objects.
type ThreeValues = [ThreeValue]

instance Binary ThreeValue where
  put = putWord8 . fromIntegral . fromEnum
  get = toEnum . fromIntegral <$> getWord8
