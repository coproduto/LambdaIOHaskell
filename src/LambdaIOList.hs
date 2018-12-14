module LambdaIOList where

import Prelude hiding (foldl')

-- listas são sequências de 0 a infinitos valores.
umaLista :: [Int]
umaLista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- listas podem ser definidas usando "ranges".
aMesmaLista :: [Int]
aMesmaLista = [1..10]

pares :: [Int]
pares = [2, 4..10]

-- e sim, listas podem ser realmente infinitas.
umaListaInfinita :: [Int]
umaListaInfinita = [1..]

infinitosImpares :: [Int]
infinitosImpares = [1, 3..]

outraListaInfinita :: [Int]
outraListaInfinita = repeat 1

-- listas podem conter qualquer tipo.
alfabeto :: [Char]
alfabeto = ['a'..'z']

-- uma String em Haskell é simplesmente uma lista de caracteres.
alfabetoDeNovo :: String
alfabetoDeNovo = ['a'..'z']

-- uma String também pode ser especificada como um literal.
inicioDoAlfabeto :: String
inicioDoAlfabeto = "abcdefghijk"

-- []
-- x :: a
-- xs :: [a]
-- x : xs

-- funções sobre listas são especificadas usando casamento de padrões
-- e recursão
somaUmLista :: Num a => [a] -> [a]
somaUmLista = map (+1)

-- funções podem especificar variáveis locais usando "where" ou "let"
dobraLista :: Num a => [a] -> [a]
dobraLista = map (*2)

-- esse padrão de aplicar uma função numa lista é tão comum que tem nome
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = (f x) : (map' f xs)

somaLista :: Num a => [a] -> a
somaLista = foldr' (+) 0

-- desafio: fazer uma função que calcula o produto de uma lista
produtoLista :: Num a => [a] -> a
produtoLista = foldr' (*) 1

-- desafio: fazer uma função que conta quantos valores numa lista são iguais a um número
contaNumero :: (Eq a, Num a) => a -> [a] -> Integer
contaNumero n []     = 0
contaNumero n (x:xs)
  | n == x    = 1 + resto
  | otherwise = resto
    where resto = contaNumero n xs

contaNumero' :: (Eq a, Num a) => a -> [a] -> Integer
contaNumero' n = foldr' go 0
  where go x acc = if x == n then 1 + acc else acc

-- o padrão de aplicar uma função para obter um "resumo" de uma lista também tem nome:
-- fold ou reduce
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ z []     = z
foldr' f z (x:xs) = f x (foldr' f z xs)

-- f = (+)
-- z = 0
-- [1, 2, 3, 4]
-- (+) 1 (foldr' (+) 0 [2,3,4])
-- (+) 1 ((+) 2 (foldr' (+) 0 [3,4]))
-- (+) 1 ((+) 2 ((+3 ((+4) 0))))

-- desafio: fazer uma função que reduz uma lista começando pela esquerda

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z []     = z
foldl' f z (x:xs) =
  let acc = f z x
  in foldl' f acc xs

-- (foldl' (+) 0 [1,2,3,4])
-- (foldl' (1 + 0) [2,3,4])
-- (foldl' (2 + 1 + 0) [3,4])
-- ...
-- (foldl' (4 + 3 + 2 + 1 + 0) [])

removePares :: [Int] -> [Int]
removePares []     = []
removePares (x:xs) =
  let resto = removePares xs
  in if x `mod` 2 /= 0 then x : resto else resto

removePares' = filter' ((/= 0) . (`mod` 2))

-- o padrão de aplicar uma função para selecionar elementos de uma lista também tem nome:
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' p (x:xs)
  | p x       = x : rest
  | otherwise = rest
    where rest = filter p xs
