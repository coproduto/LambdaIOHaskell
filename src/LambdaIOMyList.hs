module LambdaIOMyList where

import Prelude hiding (
  map,
  filter,
  foldl,
  foldr,
  flatten,
  reverse
  )
import LambdaIOOptional

data List a = Cons a (List a) | Nil

instance Show a => Show (List a) where
  show l = wrap (showValues l)
    where
      wrap s = "[ " ++ s ++ "]"

      showValues Nil = ""
      showValues (Cons a as) =
        (show a) ++ " " ++ showValues as


(+++) :: List a -> List a -> List a
Nil          +++ Nil          = Nil
Nil          +++ l@(Cons _ _) = l
l            +++ Nil          = l
(Cons a as)  +++ l2           =
  Cons a (as +++ l2)

map :: (a -> b) -> List a -> List b
map _ Nil         = Nil
map f (Cons a as) = Cons (f a) (map f as)

filter :: (a -> Bool) -> List a -> List a
filter _ Nil         = Nil
filter p (Cons a as)
  | p a       = Cons a (filter p as)
  | otherwise = filter p as

foldl :: (a -> b -> a) -> a -> List b -> a
foldl _ z Nil         = z
foldl f z (Cons x xs) =
  let acc = f z x
  in foldl f acc xs

foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ z Nil         = z
foldr f z (Cons x xs) =
  f x (foldr f z xs)

flatten :: List (List a) -> List a
flatten Nil                   = Nil
flatten (Cons Nil ls)         = flatten ls
flatten (Cons (Cons x xs) ls) =
  (Cons x (flatten (Cons xs ls)))

reverse :: List a -> List a
reverse Nil         = Nil
reverse (Cons x xs) =
  let rest = reverse xs
  in rest +++ (Cons x Nil)

listApply :: List (a -> b) -> List a -> List b
listApply Nil         _           = Nil
listApply _           Nil         = Nil
listApply (Cons f fs) (Cons x xs) =
  (Cons (f x) (listApply fs xs))

listApply2 :: List (a -> b) -> List a -> List b
listApply2 Nil _   = Nil
listApply2 _   Nil = Nil
listApply2 (Cons f fs) l =
  (map f l) +++ (listApply2 fs l)

flatMap :: (a -> List b) -> List a -> List b
flatMap f = flatten . map f

traverseList :: List (Optional a) -> Optional (List a)
traverseList Nil                = Full Nil
traverseList (Cons Empty os)    = Empty
traverseList (Cons (Full x) os) =
  case traverseList os of
    Empty  -> Empty
    Full l -> Full (Cons x l)
