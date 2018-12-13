module LambdaIOMyList where

data List a = Cons a (List a) | Nil

concat :: List a -> List a -> List a
concat []     l' = l'
concat (l:ls) l' = l : (concat ls l')

-- desafio: implementar as funções básicas de lista
-- (map, filter, folds) na nossa lista customizada

-- desafio: fazer uma função que aplica uma lista de funções
-- a uma lista de valores

flatten :: List (List a) -> List a
flatten []     = []
flatten (l:ls) = concat l (flatten ls)

-- desafio: fazer uma função que inverte uma lista

-- desafio: fazer uma função que aplica uma função que retorna lista
-- a uma lista

-- desafio: fazer uma função que transforma uma lista de Optional em
-- um Optional de uma lista
