----------------------------------------------------------------------------
----- Trabalho Prático 1 - Linguagens de Programação (DCC019 - 2024.2) -----
----------------------------- Desenvolvido por: ----------------------------
--               Jonatas Dias Machado Costa (202165077AC)
--              Maria Luísa Riolino Guimarães (202165563C)
----------------------------------------------------------------------------

module Utils
    ( emptyRow,
      endGame,
      printGame,
      removeMatches,
      generateRandomMatches,
      generateRandomNumber,
      generateRandomRows,
      getAvailableRows
    ) where

import System.Random (mkStdGen, randomR, randomRIO)

----------------------------------------------------------------------------
-- Gera números aleatórios
----------------------------------------------------------------------------

generateRandomNumber :: Int -> Int -> IO Int --
generateRandomNumber minNumber maxNumber = randomRIO(minNumber, maxNumber)

generateRandomRows :: IO Int
generateRandomRows = generateRandomNumber 2 10

generateRandomMatches :: IO [Int]
generateRandomMatches = do
  num <- generateRandomNumber 1 4
  let n = num * 2 - 1
  return $ replicate n 1 ++ replicate (7 - n) 0

----------------------------------------------------------------------------
-- Checa se a fila está vazia (contém apenas 0s)
----------------------------------------------------------------------------
emptyRow :: [Int] -> Bool
emptyRow row = all (== 0) row

----------------------------------------------------------------------------
-- Checa se não há filas restantes -> fim de jogo
----------------------------------------------------------------------------
endGame :: [[Int]] -> Bool
endGame rowsList = null rowsList

----------------------------------------------------------------------------
-- Imprime o jogo: cada `1` na lista é representado por "|" (palitinho)
----------------------------------------------------------------------------
printGame :: [[Int]] -> IO ()
printGame rowsList =
    mapM_
        ( \row -> do
            mapM_
                ( \matchstick ->
                    if matchstick == 1
                        then putStr " |"
                        else return ()
                )
                row
            putStrLn ""
        )
        rowsList

----------------------------------------------------------------------------
-- Retorna o número de linhas disponíveis
----------------------------------------------------------------------------

getAvailableRows :: [[Int]] -> Int
getAvailableRows rowsList = length rowsList

----------------------------------------------------------------------------
-- Remove os palitos de uma fileira (chosenRow) and atualiza rowsList
----------------------------------------------------------------------------
removeMatches :: [[Int]] -> Int -> Int -> [[Int]]
removeMatches rowsList chosenRow m =
    let rowIndex = chosenRow - 1
        oldRow = rowsList !! rowIndex
        newRow = removeMatchesFromRow oldRow m
        updatedRows = replaceAt rowIndex newRow rowsList
    in if emptyRow newRow
        then deleteRowAt rowIndex updatedRows
        else updatedRows

----------------------------------------------------------------------------
-- Remove os últimos 'm' palitos (i.e., altera m '1's para '0's)
----------------------------------------------------------------------------
removeMatchesFromRow :: [Int] -> Int -> [Int]
removeMatchesFromRow row m = reverse $ go m (reverse row)
    where
        go 0 xs = xs
        go _ [] = []
        go c (x : xs)
            | x == 1 && c > 0 = 0 : go (c - 1) xs
            | otherwise = x : go c xs

----------------------------------------------------------------------------
-- Substitui fileira em um índice especificado (idx)
----------------------------------------------------------------------------
replaceAt :: Int -> [Int] -> [[Int]] -> [[Int]]
replaceAt idx newRow rows =
    take idx rows ++ [newRow] ++ drop (idx + 1) rows

----------------------------------------------------------------------------
-- Deleta fileira em um índice especificado (idx)
----------------------------------------------------------------------------
deleteRowAt :: Int -> [[Int]] -> [[Int]]
deleteRowAt idx rows =
    take idx rows ++ drop (idx + 1) rows
