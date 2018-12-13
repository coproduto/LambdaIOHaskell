module LambdaIOBasics where

import Data.Int  (Int8, Int16, Int32, Int64)
import Data.Bits (bitSizeMaybe)
import Data.Char (chr, ord)

-- constantes + anotações de tipo
-- Int: inteiro nativo da máquina
vinte :: Int
vinte = 20

-- Inteiros com tamanhos
numero8Bits :: Int8
numero8Bits = 128

numero16Bits :: Int16
numero16Bits = 32768

numero32Bits :: Int32
numero32Bits = 2147483648

numero64Bits :: Int64
numero64Bits = 9223372036854775808

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

dobro :: Int16 -> Int16
dobro x = x * 2

quadrado :: Int32 -> Int32
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
numeroPar 0 = True
numeroPar n = numeroPar (n - 2)

numeroParEficiente :: Int -> Bool
numeroParEficiente x = x `mod` 2 == 0

-- funções podem ser operadores
(+*+) :: Int -> Int -> Int
a +*+ b = (a + b) * (a + b)

-- funções podem receber funções e retornar funções
duasVezes :: (Int -> Int) -> Int -> Int
duasVezes f x = f (f x)

-- desafio:
-- fazer uma função que aplica uma função qualquer número de
-- vezes a um argumento
-- nVezes :: ?
-- nVezes = ?
