-- Main module for Jogo dos Palitinhos
-- Implement game logic here

import System.Random (randomRIO)

generateRandomRows :: IO Int    

generateRandomRows = do
    randomRIO(1, 10)


-- estadoInicial :: Int -> [[Int]] [Int]