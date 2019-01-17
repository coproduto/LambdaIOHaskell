module LambdaIOIO where

import Data.Char

stringToUpper :: String -> String
stringToUpper = fmap toUpper

main :: IO ()
main = do
  line1 <- getLine 
  line2 <- getLine
  putStrLn (stringToUpper (line1 ++ line2))


-- desafio: escolher com a plateia um programa simples que se comunica
-- com o usuário para fazer

