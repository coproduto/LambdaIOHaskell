module LambdaIOTypeclasses where

import Prelude hiding (
  Functor(..),
  Applicative(..),
  Monad(..),
  (<$>),
  (=<<)
  )

import qualified LambdaIOOptional as Opt
import qualified LambdaIOMyList as Lst

data Absurd = Absurd Absurd

class MyShow a where
  myShow :: a -> String

instance MyShow () where
  myShow () = "()"

data Foo = Bar | Baz
instance MyShow Foo where
  myShow Bar = "Bar"
  myShow Baz = "Baz"

class MyEq a where
  (===) :: a -> a -> Bool
  (/==) :: a -> a -> Bool

  a === b = not (a /== b)
  a /== b = not (a === b)

  {-# MINIMAL (===) #-}
  
instance MyEq Foo where
  Bar === Bar = True
  Baz === Baz = True
  _   === _   = False

class Eq a => MyOrd a where
  myCompare :: a -> a -> Ordering

  (<<<) :: a -> a -> Bool
  a <<< b = (myCompare a b) == LT

  (>>>) :: a -> a -> Bool
  a >>> b = (myCompare a b) == GT

  

instance MyOrd Int where
  myCompare a b
    | a > b     = GT
    | a < b     = LT
    | otherwise = EQ

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Lst.List where
  fmap = Lst.map

instance Functor Opt.Optional where
  fmap = Opt.over

class Functor f => Applicative f where
  pure  :: a -> f a 
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Lst.List where
  pure x = Lst.Cons x Lst.Nil
  (<*>)  = Lst.listApply2

newtype ZipList a = ZL (Lst.List a)
instance Functor ZipList where
  fmap f (ZL l) = ZL $ fmap f l
instance Applicative ZipList where
  pure x = ZL (Lst.Cons x Lst.Nil)
  (ZL l1) <*> (ZL l2) =
    ZL $ Lst.listApply l1 l2

instance Applicative Opt.Optional where
  pure  = Opt.Full
  (<*>) = Opt.applyOptional

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

class Applicative m => Monad m where
  (=<<) :: (a -> m b) -> m a -> m b

  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) = flip (=<<)

instance Monad Opt.Optional where
  (=<<) = Opt.flatMapOptional

instance Monad Lst.List where
  (=<<) = Lst.flatMap


bar :: Int -> Lst.List Int
bar x 
  | x `mod` 2 == 0 =
    Lst.Cons x (Lst.Cons (2 * x) Lst.Nil)
  | x `mod` 2 /= 0  = Lst.Cons x Lst.Nil

foo :: Lst.List Int
foo = bar =<< nums
  where
    nums = Lst.Cons 1 (Lst.Cons 2 (Lst.Cons 3 Lst.Nil))
