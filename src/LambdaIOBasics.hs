module LambdaIOBasics where

import Prelude hiding ((.))
import Data.Int  (Int8, Int16, Int32, Int64)
import Data.Bits (bitSizeMaybe)
import Data.Char (chr, ord)

-- constantes + anotações de tipo
-- Int: inteiro nativo da máquina
vinte :: Int
vinte = 20

-- Inteiros com tamanhos
numero8Bits :: Int8
numero8Bits = 25

numero16Bits :: Int16
numero16Bits = 1000

numero32Bits :: Int32
numero32Bits = 10 ^ 5

numero64Bits :: Int64
numero64Bits = 10 ^ 10

-- Integer: inteiro de tamanho arbitrário
numeroMuitoGrande :: Integer
numeroMuitoGrande = 10 ^ 1000

-- Funções são aplicadas sem parênteses, usando apenas espaços.
inverso :: Integer
inverso = negate numeroMuitoGrande

-- Haskell não tem conversões implícitas entre tipos.
-- Todavia, existem funções de conversão genéricas.
soma :: Int16
soma = (fromIntegral numero8Bits) + numero16Bits

-- Como visto nas funções negate e fromIntegral, Haskell suporta funções
-- genéricas. Em geral é uma má prática escrever funções com tipos muito
-- específicos.

dobro :: (Num a) => a -> a
dobro x = x * 2

quadrado :: (Num a) => a -> a
quadrado x = x * x

-- O tipo char representa um caractere Unicode.

caractere :: Char
caractere = 'a'

caractereChines :: Char
caractereChines = '你'

emoji :: Char
emoji = '💩'

-- O tipo char pode ser convertido de e para um inteiro usando as funções
-- chr e ord.
codigoCaractere :: Int
codigoCaractere = ord caractere

codigoCaractereChines :: Int
codigoCaractereChines = ord caractereChines

caractereDeNovo :: Char
caractereDeNovo = chr codigoCaractere

-- funções podem ser definidas usando "casamento de padrões":
igualASete :: Int -> Bool
igualASete 7 = True
igualASete _ = False

igualALetraA :: Char -> Bool
igualALetraA 'a' = True
igualALetraA _   = False

-- funções podem ser recursivas
numeroPar :: Int -> Bool
numeroPar n
  | n < 0     = numeroPar $ negate n
  | otherwise = numeroPositivoPar n
    where numeroPositivoPar 0 = True
          numeroPositivoPar 1 = False
          numeroPositivoPar n = numeroPositivoPar (n - 2)
{-numeroPar 0 = True
numeroPar 1 = False
numeroPar n = numeroPar (n - 2)-}

numeroParEficiente :: Int -> Bool
numeroParEficiente x = x `mod` 2 == 0

-- funções podem ser operadores
(+*+) :: Int -> Int -> Int
a +*+ b = (a + b) * (a + b)

-- funções podem receber funções e retornar funções
duasVezes :: (a -> a) -> a -> a
duasVezes f = f . f

proximoCaractere :: Char -> Char
proximoCaractere = chr . (+1) . ord

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

(|>) :: (a -> b) -> (b -> c) -> a -> c
(|>) = flip (.)

-- desafio:
-- fazer uma função que aplica uma função qualquer número de
-- vezes a um argumento
nVezes :: Integral i => i -> (a -> a) -> a -> a
nVezes n f x
  | n <= 0    = x
  | otherwise = nVezes (n - 1) f (f x)

for val end f
  | end val   = val
  | otherwise = for (f val) end f
