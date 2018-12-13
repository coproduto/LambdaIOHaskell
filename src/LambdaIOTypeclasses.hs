module LambdaIOTypeclasses where

import Prelude hiding (Functor)
import LambdaIOOptional
import LambdaIOMyList

class MyShow a where
  myShow :: a -> String

class MyEq a where
  (===) :: a -> a -> Bool

class MyOrd a where
  myCompare :: a -> a -> Ordering

-- desafio: criar um tipo arbitrário com instâncias de MyEq e MyOrd

instance MyShow () where
  myShow () = "()"

-- desafio: fazer uma instância de Show p/ Optional

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- desafio: criar um tipo de dados de árvore e tornar ele uma instância
-- de Functor
