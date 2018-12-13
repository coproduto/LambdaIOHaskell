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

aMesmaListaInfinita :: [Int]
aMesmaListaInfinita = repeat 1

-- listas podem conter qualquer tipo.
alfabeto :: [Char]
alfabeto = ['a'..'z']

-- uma String em Haskell é simplesmente uma lista de caracteres.
alfabetoDeNovo :: String
alfabetoDeNovo = ['a'..'z']

-- uma String também pode ser especificada como um literal.
inicioDoAlfabeto :: String
inicioDoAlfabeto = "abcdefghijk"

-- funções sobre listas são especificadas usando casamento de padrões
-- e recursão
somaUmLista :: [Int] -> [Int]
somaUmLista []     = []
somaUmLista (x:xs) = (x + 1) : xs

-- funções podem especificar variáveis locais usando "where" ou "let"
dobraLista :: [Int] -> [Int]
dobraLista []     = []
dobraLista (x:xs) = (dobra x) : xs
  where dobra x = 2 * x

dobraLista' :: [Int] -> [Int]
dobraLista' [] = []
dobraLista' (x:xs) =
  let dobra x = 2 * x
  in (dobra x) : xs

-- esse padrão de aplicar uma função numa lista é tão comum que tem nome
map' :: (a -> b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = (f x) : (map' f xs)

somaLista :: [Int] -> Int
somaLista []     = 0
somaLista (x:xs) = x + (somaLista xs)

-- desafio: fazer uma função que calcula o produto de uma lista
-- produtoLista :: ?
-- produtoLista = ?

-- desafio: fazer uma função que conta quantos valores numa lista são iguais a um número
-- contaNumero :: ?
-- contaNumero = ?

-- o padrão de aplicar uma função para obter um "resumo" de uma lista também tem nome:
-- fold ou reduce
foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' _ z []     = z
foldr' f z (x:xs) = f x (foldl f z xs)

-- desafio: fazer uma função que reduz uma lista começando pela esquerda
-- foldl' :: ?
-- foldl' = ?

removePares :: [Int] -> [Int]
removePares []     = []
removePares (x:xs) =
  let resto = removePares xs
  in if x `mod` 2 == 0 then x : resto else resto

-- o padrão de aplicar uma função para selecionar elementos de uma lista também tem nome:
filter' :: (a -> Bool) -> [a] -> [a]
filter' p []     = []
filter' p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs
