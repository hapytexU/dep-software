{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, FlexibleContexts, Safe, TemplateHaskellQuotes #-}

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
    -- * Convert to and from a 'ThreeValue'
  , fromBool, fromMaybeBool, toUpper, toLower
    -- * Operators on 'ThreeValue'
  , opposite
    -- * Type aliasses
  , ThreeValues
    -- Parsing a ThreeValue and ThreeValues
  , parseThreeValue, parseThreeValues, parseThreeValues1
  ) where

import Control.Applicative((<|>))

import Data.Bool(bool)
import Data.Binary(Binary(put, get), getWord8, putWord8)
import Data.Data(Data)
import Data.List(find)
import Data.List.NonEmpty(NonEmpty((:|)))

import Dep.Core(Opposite(opposite), Mergeable(merge))
import Data.Default(Default(def))

import GHC.Generics(Generic)

import Language.Haskell.TH.Lib(conE)
import Language.Haskell.TH.Syntax(Lift(lift, liftTyped), TExp(TExp))

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

import Text.Parsec(ParsecT, Stream, many)
import Text.Parsec.Char(oneOf)

-- | A data type that is used if a value can present three logical values: /don't care/ (or don't know);
-- /zero/; and /one/.
data ThreeValue
  = Zero  -- ^ The value is /zero/ or /false/.
  | One  -- ^ The value is /one/ or /true/.
  | DontCare  -- ^ We do not care or do not know the value.
  deriving (Bounded, Data, Enum, Eq, Generic, Ord, Read, Show)

-- | A function that maps 'One's and 'DontCare's to 'True's; and 'Zero's to 'False'.
toUpper
  :: ThreeValue -- The given 'ThreeValue' to convert to a 'Bool'.
  -> Bool  -- A 'Bool' that is 'True' for 'One' and 'DontCare', and 'False' for 'Zero'.
toUpper Zero = False
toUpper _ = True

-- | A function that maps 'Zero's and 'DontCare's to 'False's; and 'One's to 'True'.
toLower
  :: ThreeValue -- The given 'ThreeValue' to convert to a 'Bool'.
  -> Bool  -- A 'Bool' that is 'True' for 'One', and 'False' for 'Zero' and 'DontCare'.
toLower One = True
toLower _ = False

instance Lift ThreeValue where
  liftTyped = fmap TExp . lift
  lift Zero = conE 'Zero
  lift One = conE 'One
  lift ~DontCare = conE 'DontCare

instance Opposite ThreeValue where
  opposite Zero = One
  opposite One = Zero
  opposite x = x

instance Default ThreeValue where
  def = DontCare

instance Semigroup ThreeValue where
  (<>) DontCare = id
  (<>) x = const x

instance Monoid ThreeValue where
  mempty = DontCare
  mconcat xs
    | Just x <- find (DontCare /=) xs = x
    | otherwise = DontCare

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

-- | A parser that can parse a single 'ThreeValue' from a 'Char'acter.
-- The characters for 'Zero' are @0@, @f@ or @F@;
-- the characters for 'Zero' are @1@, @t@ or @T@; and
-- the characters for 'DontCare' are @-@, @d@ or @D@,
parseThreeValue :: Stream s m Char => ParsecT s u m ThreeValue
parseThreeValue = (Zero <$ oneOf "0fF") <|> (One <$ oneOf "1tT") <|> (DontCare <$ oneOf "-dD")

-- | A parser that can parse a (possibly empty) list of 'ThreeValue'
-- from a 'Char'acter. The characters for 'Zero' are @0@, @f@ or @F@;
-- the characters for 'Zero' are @1@, @t@ or @T@; and
-- the characters for 'DontCare' are @-@, @d@ or @D@,
parseThreeValues :: Stream s m Char => ParsecT s u m ThreeValues
parseThreeValues = many parseThreeValue

-- | A parser that can parse a 'NonEmpty' list of 'ThreeValue'
-- from a 'Char'acter. The characters for 'Zero' are @0@, @f@ or @F@;
-- the characters for 'Zero' are @1@, @t@ or @T@; and
-- the characters for 'DontCare' are @-@, @d@ or @D@,
parseThreeValues1 :: Stream s m Char => ParsecT s u m (NonEmpty ThreeValue)
parseThreeValues1 = (:|) <$> parseThreeValue <*> parseThreeValues
