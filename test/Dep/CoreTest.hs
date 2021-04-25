{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Dep.CoreTest where

import Data.Typeable(Typeable, typeOf)

import Test.Hspec
import Test.QuickCheck

toParen :: String -> String
toParen s
  | ' ' `elem` s = '(' : s ++ ")"
  | otherwise = s

instanceText :: (Typeable a) => String -> a -> String
instanceText cls a = "\ESC[1;34minstance\ESC[0m \ESC[1m" ++ cls ++ "\ESC[0m " ++ toParen (show (typeOf a))

testFunctor :: forall f a b c . (Functor f, Typeable f, Typeable a, Arbitrary a, CoArbitrary a, Function a, Show a, Arbitrary b, CoArbitrary b, Function b, Show b, Arbitrary c, CoArbitrary c, Show c, Arbitrary (f a), Eq (f a), Show (f a), Eq (f c)) => SpecWith ()
testFunctor = describe (instanceText "Functor" (undefined :: f a)) do
  it "Identity of the functor" (property (testFunctorIdentity @f @a))
  it "Composition of the functor" (property (testFunctorComposition @f @a @b @c))

testFunctorIdentity :: forall f a . (Functor f, CoArbitrary a, Eq (f a)) => f a -> Bool
testFunctorIdentity x = fmap id x == x

testFunctorComposition :: forall f a b c . (Functor f, CoArbitrary b, Eq (f a), Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
testFunctorComposition x g f = fmap (applyFun f . applyFun g) x == fmap (applyFun f) (fmap (applyFun g) x)
