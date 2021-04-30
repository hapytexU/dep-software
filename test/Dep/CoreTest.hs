{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Dep.CoreTest where

import Control.Applicative(liftA2)

import Data.Binary(Binary, decode, encode)
import Data.Proxy(Proxy(Proxy))
import Data.Typeable(Typeable, typeRep)

import Test.Hspec
import Test.QuickCheck(Arbitrary, CoArbitrary, Fun, Function, Testable, applyFun, applyFun2, property)

toParen :: String -> String
toParen s
  | ' ' `elem` s = '(' : s ++ ")"
  | otherwise = s

instanceDesc :: forall a . Typeable a => String -> SpecWith () -> SpecWith ()
instanceDesc cls = describe ("\ESC[1;34minstance\ESC[0m \ESC[1m" ++ cls ++ "\ESC[0m " ++ toParen (show (typeRep (Proxy :: Proxy a))))

itproperty :: Testable prop => String -> prop -> SpecWith ()
itproperty s t = it s (property t)

testFunctorLaws :: forall f a b c . (Functor f, Typeable f
  , Typeable a, Arbitrary a, CoArbitrary a, Function a, Show a
  , Typeable b, Arbitrary b, CoArbitrary b, Function b, Show b
  , Typeable c, Arbitrary c, CoArbitrary c, Function c, Show c
  , Arbitrary (f a), Eq (f a), Show (f a)
  , Arbitrary (f b), Eq (f b), Show (f b)
  , Arbitrary (f c), Eq (f c), Show (f c)) => SpecWith ()
testFunctorLaws = describe "Functor laws" do
  instanceDesc @ (f a) "Functor" do
    itproperty "Identity of the functor 1" (testFunctorIdentity @f @a)
    itproperty "Composition of the functor 1" (testFunctorComposition @f @a @b @c)
    itproperty "Composition of the functor 2" (testFunctorComposition @f @a @c @b)
    itproperty "Test the left fmap 1" (testFunctorLeftMap @f @a @b)
    itproperty "Test the left fmap 2" (testFunctorLeftMap @f @a @c)
  instanceDesc @ (f b) "Functor" do
    itproperty "Identity of the functor 2" (testFunctorIdentity @f @b)
    itproperty "Composition of the functor 3" (testFunctorComposition @f @b @a @c)
    itproperty "Composition of the functor 4" (testFunctorComposition @f @b @c @a)
    itproperty "Test the left fmap 3" (testFunctorLeftMap @f @b @a)
    itproperty "Test the left fmap 4" (testFunctorLeftMap @f @b @c)
  instanceDesc @ (f c) "Functor" do
    itproperty "Identity of the functor 3" (testFunctorIdentity @f @c)
    itproperty "Composition of the functor 5" (testFunctorComposition @f @c @a @b)
    itproperty "Composition of the functor 6" (testFunctorComposition @f @c @b @a)
    itproperty "Test the left fmap 5" (testFunctorLeftMap @f @c @a)
    itproperty "Test the left fmap 6" (testFunctorLeftMap @f @c @a)

testApplicativeLaws :: forall f a b c . (
    Applicative f, Typeable f
  , Typeable a, Arbitrary a, CoArbitrary a, Function a, Show a
  , Typeable b, Arbitrary b, CoArbitrary b, Function b, Show b
  , Typeable c, Arbitrary c, CoArbitrary c, Function c, Show c
  , Arbitrary (f a), Eq (f a), Show (f a)
  , Arbitrary (f b), Eq (f b), Show (f b)
  , Arbitrary (f c), Eq (f c), Show (f c)
  , Arbitrary (f (Fun a b)), Show (f (Fun a b))
  , Arbitrary (f (Fun a c)), Show (f (Fun a c))
  , Arbitrary (f (Fun b a)), Show (f (Fun b a))
  , Arbitrary (f (Fun b c)), Show (f (Fun b c))
  , Arbitrary (f (Fun c a)), Show (f (Fun c a))
  , Arbitrary (f (Fun c b)), Show (f (Fun c b))) => SpecWith ()
testApplicativeLaws = describe "Applicative laws" do
  instanceDesc @ (f a) "Applicative" do
    itproperty "Equivalence of (<*>) = liftA2 id 1" (testApplicativeSequentialApplication @f @a @b)
    itproperty "Equivalence of (<*>) = liftA2 id 2" (testApplicativeSequentialApplication @f @a @c)
    itproperty "Equivalence of LiftA2 f x y = f <$> x <*> y 1" (testApplicativeLiftA2 @f @a @b @c)
    itproperty "Equivalence of LiftA2 f x y = f <$> x <*> y 2" (testApplicativeLiftA2 @f @a @c @b)
    itproperty "Identity of the applicative 1" (testApplicativeIdentity @f @a)
    itproperty "Composition of the applicative 1" (testApplicativeComposition @f @a @b @c)
    itproperty "Composition of the applicative 2" (testApplicativeComposition @f @a @c @b)
    itproperty "Homomorphism of the applicative 1" (testApplicativeHomomorphism @f @a @b)
    itproperty "Homomorphism of the applicative 2" (testApplicativeHomomorphism @f @a @c)
    itproperty "Interchange of the applicative 1" (testApplicativeInterchange @f @a @b)
    itproperty "Interchange of the applicative 2" (testApplicativeInterchange @f @a @c)
    itproperty "Correct left sequence 1" (testApplicativeLeftSequence @f @a @b)
    itproperty "Correct left sequence 2" (testApplicativeLeftSequence @f @a @c)
    itproperty "Correct right sequence 1" (testApplicativeRightSequence @f @a @b)
    itproperty "Correct right sequence 2" (testApplicativeRightSequence @f @a @c)
  instanceDesc @ (f b) "Applicative" do
    itproperty "Equivalence of (<*>) = liftA2 id 3" (testApplicativeSequentialApplication @f @b @a)
    itproperty "Equivalence of (<*>) = liftA2 id 4" (testApplicativeSequentialApplication @f @b @c)
    itproperty "Equivalence of LiftA2 f x y = f <$> x <*> y 3" (testApplicativeLiftA2 @f @b @a @c)
    itproperty "Equivalence of LiftA2 f x y = f <$> x <*> y 4" (testApplicativeLiftA2 @f @b @c @a)
    itproperty "Identity of the applicative 2" (testApplicativeIdentity @f @b)
    itproperty "Composition of the applicative 3" (testApplicativeComposition @f @b @a @c)
    itproperty "Composition of the applicative 4" (testApplicativeComposition @f @b @c @a)
    itproperty "Homomorphism of the applicative 3" (testApplicativeHomomorphism @f @b @a)
    itproperty "Homomorphism of the applicative 4" (testApplicativeHomomorphism @f @b @c)
    itproperty "Interchange of the applicative 3" (testApplicativeInterchange @f @b @a)
    itproperty "Interchange of the applicative 4" (testApplicativeInterchange @f @b @c)
    itproperty "Correct left sequence 5" (testApplicativeLeftSequence @f @b @a)
    itproperty "Correct left sequence 6" (testApplicativeLeftSequence @f @b @c)
    itproperty "Correct right sequence 5" (testApplicativeRightSequence @f @b @a)
    itproperty "Correct right sequence 6" (testApplicativeRightSequence @f @b @c)
  instanceDesc @ (f c) "Applicative" do
    itproperty "Equivalence of (<*>) = liftA2 id 5" (testApplicativeSequentialApplication @f @c @a)
    itproperty "Equivalence of (<*>) = liftA2 id 6" (testApplicativeSequentialApplication @f @c @b)
    itproperty "Equivalence of LiftA2 f x y = f <$> x <*> y 5" (testApplicativeLiftA2 @f @c @a @b)
    itproperty "Equivalence of LiftA2 f x y = f <$> x <*> y 6" (testApplicativeLiftA2 @f @c @b @a)
    itproperty "Identity of the applicative 3" (testApplicativeIdentity @f @c)
    itproperty "Composition of the applicative 5" (testApplicativeComposition @f @c @a @b)
    itproperty "Composition of the applicative 6" (testApplicativeComposition @f @c @b @a)
    itproperty "Homomorphism of the applicative 5" (testApplicativeHomomorphism @f @c @a)
    itproperty "Homomorphism of the applicative 6" (testApplicativeHomomorphism @f @c @b)
    itproperty "Interchange of the applicative 5" (testApplicativeInterchange @f @c @a)
    itproperty "Interchange of the applicative 6" (testApplicativeInterchange @f @c @b)
    itproperty "Correct left sequence 5" (testApplicativeLeftSequence @f @c @a)
    itproperty "Correct left sequence 5" (testApplicativeLeftSequence @f @c @a)
    itproperty "Correct right sequence 6" (testApplicativeRightSequence @f @c @b)
    itproperty "Correct right sequence 6" (testApplicativeRightSequence @f @c @b)

testMonadLaws :: forall f a b c . (
    Monad f, Typeable f
  , Typeable a, Arbitrary a, CoArbitrary a, Function a, Show a
  , Typeable b, Arbitrary b, CoArbitrary b, Function b, Show b
  , Typeable c, Arbitrary c, CoArbitrary c, Function c, Show c
  , Arbitrary (f a), Eq (f a), Show (f a)
  , Arbitrary (f b), Eq (f b), Show (f b)
  , Arbitrary (f c), Eq (f c), Show (f c)
  , Arbitrary (f (Fun a b)), Show (f (Fun a b))
  , Arbitrary (f (Fun a c)), Show (f (Fun a c))
  , Arbitrary (f (Fun b a)), Show (f (Fun b a))
  , Arbitrary (f (Fun b c)), Show (f (Fun b c))
  , Arbitrary (f (Fun c a)), Show (f (Fun c a))
  , Arbitrary (f (Fun c b)), Show (f (Fun c b))) => SpecWith ()
testMonadLaws = describe "Monad laws" do
  instanceDesc @ (f a) "Monad" do
    itproperty "Left identity for monads 1" (testMonadLeftIdentity @f @a @b)
    itproperty "Left identity for monads 2" (testMonadLeftIdentity @f @a @c)
    itproperty "Right identity for monads 1" (testMonadRightIdentity @f @a)
    itproperty "Associativity for monads 1" (testMonadAssociativity @f @a @b @c)
    itproperty "Associativity for monads 2" (testMonadAssociativity @f @a @c @b)
    itproperty "Right sequence is equivalent to the then operator 1" (testMonadEquivalentThen @f @a @b)
    itproperty "Right sequence is equivalent to the then operator 2" (testMonadEquivalentThen @f @a @c)
    itproperty "Test that pure has the same effect as return 1" (testMonadPureIsReturn @f @a)
    itproperty "Test that the bind operator is the same as sequential application 1" (testMonadOperatorEqualToSequentialApplication @f @a @b)
    itproperty "Test that the bind operator is the same as sequential application 2" (testMonadOperatorEqualToSequentialApplication @f @a @c)
    itproperty "Test that fmap is equivalent to using a bind with a function application and return 1" (testMonadFmapIsSequenceWithReturn @f @a @b)
    itproperty "Test that fmap is equivalent to using a bind with a function application and return 2" (testMonadFmapIsSequenceWithReturn @f @a @c)
    itproperty "Test monad bind equality 1" (testMonadBindEquality @f @a @b)
    itproperty "Test monad bind equality 2" (testMonadBindEquality @f @a @c)
  instanceDesc @ (f b) "Monad" do
    itproperty "Left identity for monads 3" (testMonadLeftIdentity @f @b @a)
    itproperty "Left identity for monads 4" (testMonadLeftIdentity @f @b @c)
    itproperty "Right identity for monads 2" (testMonadRightIdentity @f @b)
    itproperty "Associativity for monads 3" (testMonadAssociativity @f @b @a @c)
    itproperty "Associativity for monads 4" (testMonadAssociativity @f @b @c @a)
    itproperty "Right sequence is equivalent to the then operator 3" (testMonadEquivalentThen @f @b @a)
    itproperty "Right sequence is equivalent to the then operator 4" (testMonadEquivalentThen @f @b @c)
    itproperty "Test that pure has the same effect as return 2" (testMonadPureIsReturn @f @b)
    itproperty "Test that the bind operator is the same as sequential application 3" (testMonadOperatorEqualToSequentialApplication @f @b @a)
    itproperty "Test that the bind operator is the same as sequential application 4" (testMonadOperatorEqualToSequentialApplication @f @b @c)
    itproperty "Test that fmap is equivalent to using a bind with a function application and return 3" (testMonadFmapIsSequenceWithReturn @f @b @a)
    itproperty "Test that fmap is equivalent to using a bind with a function application and return 4" (testMonadFmapIsSequenceWithReturn @f @b @c)
    itproperty "Test monad bind equality 3" (testMonadBindEquality @f @b @a)
    itproperty "Test monad bind equality 4" (testMonadBindEquality @f @b @c)
  instanceDesc @ (f c) "Monad" do
    itproperty "Left identity for monads 5" (testMonadLeftIdentity @f @c @a)
    itproperty "Left identity for monads 6" (testMonadLeftIdentity @f @c @b)
    itproperty "Right identity for monads 3" (testMonadRightIdentity @f @c)
    itproperty "Associativity for monads 1" (testMonadAssociativity @f @c @a @b)
    itproperty "Associativity for monads 2" (testMonadAssociativity @f @c @b @a)
    itproperty "Right sequence is equivalent to the then operator 5" (testMonadEquivalentThen @f @c @a)
    itproperty "Right sequence is equivalent to the then operator 6" (testMonadEquivalentThen @f @c @b)
    itproperty "Test that pure has the same effect as return 3" (testMonadPureIsReturn @f @c)
    itproperty "Test that the bind operator is the same as sequential application 5" (testMonadOperatorEqualToSequentialApplication @f @c @a)
    itproperty "Test that the bind operator is the same as sequential application 6" (testMonadOperatorEqualToSequentialApplication @f @c @b)
    itproperty "Test that fmap is equivalent to using a bind with a function application and return 5" (testMonadFmapIsSequenceWithReturn @f @c @a)
    itproperty "Test that fmap is equivalent to using a bind with a function application and return 6" (testMonadFmapIsSequenceWithReturn @f @c @b)
    itproperty "Test monad bind equality 5" (testMonadBindEquality @f @c @a)
    itproperty "Test monad bind equality 6" (testMonadBindEquality @f @c @b)

testBinaryLaws :: forall a . (Arbitrary a, Binary a, Eq a, Show a, Typeable a) => SpecWith ()
testBinaryLaws = describe "Binary laws" $
  instanceDesc @a "" $
    itproperty "Check if decoding an encoded object will yield the same object" (testBinaryIdentity @a)

testSemigroupLaws :: forall s . (Arbitrary s, Eq s, Semigroup s, Show s, Typeable s) => SpecWith ()
testSemigroupLaws = describe "Semigroup laws" do
  instanceDesc @ s "Semigroup" $
    itproperty "Associativity of the semigroup 1" (testSemigroupAssociativity @s)

testMonoidLaws :: forall m . (Arbitrary m, Eq m, Monoid m, Show m, Typeable m) => SpecWith ()
testMonoidLaws = describe "Monoid laws" do
  instanceDesc @ m "Monoid" $ do
    itproperty "Right identity of a monoid 1" (testMonoidRightIdentity @m)
    itproperty "Left identity of a monoid 1" (testMonoidLeftIdentity @m)
    itproperty "Concatenation of the monoid 1" (testMonoidConcatenation @m)


_unFun :: Functor f => Fun a b -> f a -> f b
_unFun = fmap . applyFun

_toFunc :: Functor f => f (Fun a b) -> f (a -> b)
_toFunc = fmap applyFun

testFunctorIdentity :: forall f a . (Functor f, Eq (f a)) => f a -> Bool
testFunctorIdentity x = fmap id x == x

testFunctorComposition :: forall f a b c . (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
testFunctorComposition x f g = fmap (applyFun g . applyFun f) x == (_unFun g) (_unFun f x)

testFunctorLeftMap :: forall f a b . (Functor f, Eq (f b)) => b -> f a -> Bool
testFunctorLeftMap x fb = fmap (const x) fb == (x <$ fb)

testApplicativeSequentialApplication :: forall f a b . (Applicative f, Eq (f b)) => f (Fun a b) -> f a -> Bool
testApplicativeSequentialApplication x y = (_toFunc x <*> y) == liftA2 id (_toFunc x) y

testApplicativeLiftA2 :: forall f a b c . (Applicative f, Eq (f c)) => Fun (a, b) c -> f a -> f b -> Bool
testApplicativeLiftA2 f x y = liftA2 (applyFun2 f) x y == (applyFun2 f <$> x <*> y)

testApplicativeIdentity :: forall f a . (Applicative f, Eq (f a)) => f a -> Bool
testApplicativeIdentity x = (pure id <*> x) == x

testApplicativeComposition :: forall f a b c . (Applicative f, Eq (f c)) => f (Fun b c) -> f (Fun a b) -> f a -> Bool
testApplicativeComposition u v w = (pure (.) <*> tu <*> tv <*> w) == (tu <*> (tv <*> w))
  where tu = _toFunc u; tv = _toFunc v

testApplicativeHomomorphism :: forall f a b . (Applicative f, Eq (f b)) => (Fun a b) -> a -> Bool
testApplicativeHomomorphism f x = (pure @f (applyFun f) <*> pure x) == pure @f (applyFun f x)

testApplicativeInterchange :: forall f a b . (Applicative f, Eq (f b)) => f (Fun a b) -> a -> Bool
testApplicativeInterchange u y = (tu <*> pure y) == (pure ($ y) <*> tu)
  where tu = _toFunc u

testApplicativeRightSequence :: forall f a b . (Applicative f, Eq (f b)) => f a -> f b -> Bool
testApplicativeRightSequence u v = (u *> v) == ((id <$ u) <*> v)

testApplicativeLeftSequence :: forall f a b . (Applicative f, Eq (f a)) => f a -> f b -> Bool
testApplicativeLeftSequence u v = (u <* v) == liftA2 const u v

testMonadLeftIdentity :: forall m a b . (Monad m, Eq (m b)) => a -> Fun a (m b) -> Bool
testMonadLeftIdentity a k = (return a >>= applyFun k) == applyFun k a

testMonadRightIdentity :: forall m a . (Monad m, Eq (m a)) => m a -> Bool
testMonadRightIdentity m = (m >>= return) == m

testMonadAssociativity :: forall m a b c . (Monad m, Eq (m c)) => m a -> Fun a (m b) -> Fun b (m c) -> Bool
testMonadAssociativity m k h = (m >>= (\x -> applyFun k x >>= applyFun h)) == ((m >>= applyFun k) >>= applyFun h)

testMonadEquivalentThen :: forall m a b . (Monad m, Eq (m b)) => m a -> m b -> Bool
testMonadEquivalentThen u v = (u *> v) == (u >> v)

testMonadPureIsReturn :: forall m a . (Monad m, Eq (m a)) => a -> Bool
testMonadPureIsReturn x = pure @m @a x == return @m @a x

testMonadOperatorEqualToSequentialApplication :: forall m a b . (Monad m, Eq (m b)) => m (Fun a b) -> m a -> Bool
testMonadOperatorEqualToSequentialApplication m1 m2 = (u1 <*> m2) == (u1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2))))
  where u1 = _toFunc m1

testMonadFmapIsSequenceWithReturn :: forall m a b . (Monad m, Eq (m b)) => Fun a b -> m a -> Bool
testMonadFmapIsSequenceWithReturn f xs = fmap ff xs == (xs >>= return . ff)
    where ff = applyFun f

testMonadBindEquality :: forall m a b . (Monad m, Eq (m b)) => m a -> m b -> Bool
testMonadBindEquality ma mb = (ma >>= \_ -> mb) == (ma >> mb)

testSemigroupAssociativity :: forall s . (Semigroup s, Eq s) => s -> s -> s -> Bool
testSemigroupAssociativity x y z = x <> (y <> z) == (x <> y) <> z

testMonoidRightIdentity :: forall m . (Monoid m, Eq m) => m -> Bool
testMonoidRightIdentity m = m <> mempty == m

testMonoidLeftIdentity :: forall m . (Monoid m, Eq m) => m -> Bool
testMonoidLeftIdentity m = mempty <> m == m

testMonoidConcatenation :: forall m . (Monoid m, Eq m) => [m] -> Bool
testMonoidConcatenation ms = foldr (<>) mempty ms == mconcat ms

testBinaryIdentity :: forall a . (Binary a, Eq a) => a -> Bool
testBinaryIdentity x = decode (encode x) == x
