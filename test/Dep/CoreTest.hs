{-# LANGUAGE AllowAmbiguousTypes, KindSignatures, ScopedTypeVariables, TypeApplications #-}

module Dep.CoreTest where

import Control.Applicative(liftA2)

import Data.Binary(Binary, decode, encode)
import Data.Proxy(Proxy(Proxy))
import Data.Typeable(Typeable, typeRep)

import Test.Hspec(Spec, SpecWith, describe, it)
import Test.QuickCheck(Arbitrary, CoArbitrary, Fun, Function, Testable, applyFun, applyFun2, property)

toParen :: String -> String
toParen s
  | ' ' `elem` s = '(' : s ++ ")"
  | otherwise = s

instanceDesc :: forall a b . Typeable a => String -> SpecWith b -> SpecWith b
instanceDesc cls = describe ("\ESC[1;34minstance\ESC[0m \ESC[1m" ++ cls ++ "\ESC[0m " ++ toParen (show (typeRep (Proxy :: Proxy a))))

instanceDesc1 :: forall (a :: * -> Constraint) b . Typeable a => String -> SpecWith b -> SpecWith b
instanceDesc1 cls = istanceDesc @a @b


itproperty :: Testable prop => String -> prop -> Spec
itproperty s t = it s (property t)

testFunctorLaws :: forall f a b c . (Functor f, Typeable f
  , Typeable a, Arbitrary a, CoArbitrary a, Function a, Show a
  , Typeable b, Arbitrary b, CoArbitrary b, Function b, Show b
  , Typeable c, Arbitrary c, CoArbitrary c, Function c, Show c
  , Arbitrary (f a), Eq (f a), Show (f a)
  , Arbitrary (f b), Eq (f b), Show (f b)
  , Arbitrary (f c), Eq (f c), Show (f c)) => SpecWith ()
testFunctorLaws = describe "Functor laws" do
  instanceDesc1 @f "Functor" do
    it "Identity of the functor 1" (property (testFunctorIdentity @f @a))
    it "Composition of the functor 1" (property (testFunctorComposition @f @a @b @c))
    it "Composition of the functor 2" (property (testFunctorComposition @f @a @c @b))
    itproperty "Test the left fmap 1" (testFunctorLeftMap @f @a @b)
    itproperty "Test the left fmap 2" (testFunctorLeftMap @f @a @c)
  instanceDesc1 @f "Functor" do
    it "Identity of the functor 2" (property (testFunctorIdentity @f @b))
    it "Composition of the functor 3" (property (testFunctorComposition @f @b @a @c))
    it "Composition of the functor 4" (property (testFunctorComposition @f @b @c @a))
    itproperty "Test the left fmap 3" (testFunctorLeftMap @f @b @a)
    itproperty "Test the left fmap 4" (testFunctorLeftMap @f @b @c)
  instanceDesc1 @f "Functor" do
    it "Identity of the functor 3" (property (testFunctorIdentity @f @c))
    it "Composition of the functor 5" (property (testFunctorComposition @f @c @a @b))
    it "Composition of the functor 6" (property (testFunctorComposition @f @c @b @a))
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
  instanceDesc1 @f "Applicative" $ do
    it "Equivalence of (<*>) = liftA2 id 1" (property (testApplicativeSequentialApplication @f @a @b))
    it "Equivalence of (<*>) = liftA2 id 2" (property (testApplicativeSequentialApplication @f @a @c))
    it "Equivalence of LiftA2 f x y = f <$> x <*> y 1" (property (testApplicativeLiftA2 @f @a @b @c))
    it "Equivalence of LiftA2 f x y = f <$> x <*> y 2" (property (testApplicativeLiftA2 @f @a @c @b))
    it "Identity of the applicative 1" (property (testApplicativeIdentity @f @a))
    it "Composition of the applicative 1" (property (testApplicativeComposition @f @a @b @c))
    it "Composition of the applicative 2" (property (testApplicativeComposition @f @a @c @b))
    it "Homomorphism of the applicative 1" (property (testApplicativeHomomorphism @f @a @b))
    it "Homomorphism of the applicative 2" (property (testApplicativeHomomorphism @f @a @c))
    it "Interchange of the applicative 1" (property (testApplicativeInterchange @f @a @b))
    it "Interchange of the applicative 2" (property (testApplicativeInterchange @f @a @c))
    it "Correct left sequence 1" (property (testApplicativeLeftSequence @f @a @b))
    it "Correct left sequence 2" (property (testApplicativeLeftSequence @f @a @c))
    it "Correct right sequence 1" (property (testApplicativeRightSequence @f @a @b))
    it "Correct right sequence 2" (property (testApplicativeRightSequence @f @a @c))
  instanceDesc1 @f "Applicative" $ do
    it "Equivalence of (<*>) = liftA2 id 3" (property (testApplicativeSequentialApplication @f @b @a))
    it "Equivalence of (<*>) = liftA2 id 4" (property (testApplicativeSequentialApplication @f @b @c))
    it "Equivalence of LiftA2 f x y = f <$> x <*> y 3" (property (testApplicativeLiftA2 @f @b @a @c))
    it "Equivalence of LiftA2 f x y = f <$> x <*> y 4" (property (testApplicativeLiftA2 @f @b @c @a))
    it "Identity of the applicative 2" (property (testApplicativeIdentity @f @b))
    it "Composition of the applicative 3" (property (testApplicativeComposition @f @b @a @c))
    it "Composition of the applicative 4" (property (testApplicativeComposition @f @b @c @a))
    it "Homomorphism of the applicative 3" (property (testApplicativeHomomorphism @f @b @a))
    it "Homomorphism of the applicative 4" (property (testApplicativeHomomorphism @f @b @c))
    it "Interchange of the applicative 3" (property (testApplicativeInterchange @f @b @a))
    it "Interchange of the applicative 4" (property (testApplicativeInterchange @f @b @c))
    it "Correct left sequence 5" (property (testApplicativeLeftSequence @f @b @a))
    it "Correct left sequence 6" (property (testApplicativeLeftSequence @f @b @c))
    it "Correct right sequence 5" (property (testApplicativeRightSequence @f @b @a))
    it "Correct right sequence 6" (property (testApplicativeRightSequence @f @b @c))
  instanceDesc1 @f "Applicative" $ do
    it "Equivalence of (<*>) = liftA2 id 5" (property (testApplicativeSequentialApplication @f @c @a))
    it "Equivalence of (<*>) = liftA2 id 6" (property (testApplicativeSequentialApplication @f @c @b))
    it "Equivalence of LiftA2 f x y = f <$> x <*> y 5" (property (testApplicativeLiftA2 @f @c @a @b))
    it "Equivalence of LiftA2 f x y = f <$> x <*> y 6" (property (testApplicativeLiftA2 @f @c @b @a))
    it "Identity of the applicative 3" (property (testApplicativeIdentity @f @c))
    it "Composition of the applicative 5" (property (testApplicativeComposition @f @c @a @b))
    it "Composition of the applicative 6" (property (testApplicativeComposition @f @c @b @a))
    it "Homomorphism of the applicative 5" (property (testApplicativeHomomorphism @f @c @a))
    it "Homomorphism of the applicative 6" (property (testApplicativeHomomorphism @f @c @b))
    it "Interchange of the applicative 5" (property (testApplicativeInterchange @f @c @a))
    it "Interchange of the applicative 6" (property (testApplicativeInterchange @f @c @b))
    it "Correct left sequence 5" (property (testApplicativeLeftSequence @f @c @a))
    it "Correct left sequence 5" (property (testApplicativeLeftSequence @f @c @a))
    it "Correct right sequence 6" (property (testApplicativeRightSequence @f @c @b))
    it "Correct right sequence 6" (property (testApplicativeRightSequence @f @c @b))

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
testMonadLaws = describe "Monad laws" $ do
  instanceDesc1 @f "Monad" $ do
    it "Left identity for monads 1" (property (testMonadLeftIdentity @f @a @b))
    it "Left identity for monads 2" (property (testMonadLeftIdentity @f @a @c))
    it "Right identity for monads 1" (property (testMonadRightIdentity @f @a))
    it "Associativity for monads 1" (property (testMonadAssociativity @f @a @b @c))
    it "Associativity for monads 2" (property (testMonadAssociativity @f @a @c @b))
    it "Right sequence is equivalent to the then operator 1" (property (testMonadEquivalentThen @f @a @b))
    it "Right sequence is equivalent to the then operator 2" (property (testMonadEquivalentThen @f @a @c))
    it "Test that pure has the same effect as return 1" (property (testMonadPureIsReturn @f @a))
    it "Test that the bind operator is the same as sequential application 1" (property (testMonadOperatorEqualToSequentialApplication @f @a @b))
    it "Test that the bind operator is the same as sequential application 2" (property (testMonadOperatorEqualToSequentialApplication @f @a @c))
    it "Test that fmap is equivalent to using a bind with a function application and return 1" (property (testMonadFmapIsSequenceWithReturn @f @a @b))
    it "Test that fmap is equivalent to using a bind with a function application and return 2" (property (testMonadFmapIsSequenceWithReturn @f @a @c))
    it "Test monad bind equality 1" (property (testMonadBindEquality @f @a @b))
    it "Test monad bind equality 2" (property (testMonadBindEquality @f @a @c))
  instanceDesc1 @f "Monad" $ do
    it "Left identity for monads 3" (property (testMonadLeftIdentity @f @b @a))
    it "Left identity for monads 4" (property (testMonadLeftIdentity @f @b @c))
    it "Right identity for monads 2" (property (testMonadRightIdentity @f @b))
    it "Associativity for monads 3" (property (testMonadAssociativity @f @b @a @c))
    it "Associativity for monads 4" (property (testMonadAssociativity @f @b @c @a))
    it "Right sequence is equivalent to the then operator 3" (property (testMonadEquivalentThen @f @b @a))
    it "Right sequence is equivalent to the then operator 4" (property (testMonadEquivalentThen @f @b @c))
    it "Test that pure has the same effect as return 2" (property (testMonadPureIsReturn @f @b))
    it "Test that the bind operator is the same as sequential application 3" (property (testMonadOperatorEqualToSequentialApplication @f @b @a))
    it "Test that the bind operator is the same as sequential application 4" (property (testMonadOperatorEqualToSequentialApplication @f @b @c))
    it "Test that fmap is equivalent to using a bind with a function application and return 3" (property (testMonadFmapIsSequenceWithReturn @f @b @a))
    it "Test that fmap is equivalent to using a bind with a function application and return 4" (property (testMonadFmapIsSequenceWithReturn @f @b @c))
    it "Test monad bind equality 3" (property (testMonadBindEquality @f @b @a))
    it "Test monad bind equality 4" (property (testMonadBindEquality @f @b @c))
  instanceDesc1 @f "Monad" $ do
    it "Left identity for monads 5" (property (testMonadLeftIdentity @f @c @a))
    it "Left identity for monads 6" (property (testMonadLeftIdentity @f @c @b))
    it "Right identity for monads 3" (property (testMonadRightIdentity @f @c))
    it "Associativity for monads 1" (property (testMonadAssociativity @f @c @a @b))
    it "Associativity for monads 2" (property (testMonadAssociativity @f @c @b @a))
    it "Right sequence is equivalent to the then operator 5" (property (testMonadEquivalentThen @f @c @a))
    it "Right sequence is equivalent to the then operator 6" (property (testMonadEquivalentThen @f @c @b))
    it "Test that pure has the same effect as return 3" (property (testMonadPureIsReturn @f @c))
    it "Test that the bind operator is the same as sequential application 5" (property (testMonadOperatorEqualToSequentialApplication @f @c @a))
    it "Test that the bind operator is the same as sequential application 6" (property (testMonadOperatorEqualToSequentialApplication @f @c @b))
    it "Test that fmap is equivalent to using a bind with a function application and return 5" (property (testMonadFmapIsSequenceWithReturn @f @c @a))
    it "Test that fmap is equivalent to using a bind with a function application and return 6" (property (testMonadFmapIsSequenceWithReturn @f @c @b))
    it "Test monad bind equality 5" (property (testMonadBindEquality @f @c @a))
    it "Test monad bind equality 6" (property (testMonadBindEquality @f @c @b))

testSemigroupLaws :: forall s . (Arbitrary s, Eq s, Semigroup s, Show s, Typeable s) => SpecWith ()
testSemigroupLaws = describe "Semigroup laws" do
  instanceDesc @ s "Semigroup" $
    it "Associativity of the semigroup 1" (property (testSemigroupAssociativity @s))

testMonoidLaws :: forall m . (Arbitrary m, Eq m, Monoid m, Show m, Typeable m) => SpecWith ()
testMonoidLaws = describe "Monoid laws" do
  instanceDesc @ m "Monoid" $ do
    it "Right identity of a monoid 1" (property (testMonoidRightIdentity @m))
    it "Left identity of a monoid 1" (property (testMonoidLeftIdentity @m))
    it "Concatenation of the monoid 1" (property (testMonoidConcatenation @m))

testBinaryLaws :: forall a . (Arbitrary a, Binary a, Eq a, Show a, Typeable a) => SpecWith ()
testBinaryLaws = describe "Binary laws" $
  instanceDesc @a "" $
    itproperty "Check if decoding an encoded object will yield the same object" (testBinaryIdentity @a)


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
