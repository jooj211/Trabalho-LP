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
      getAvailableRows,
      processRowToBinary,
      decimalToBinary,
      binaryToDecimal,
      transposeAndSum,
      normalizeRows,
      findBestOption,
      checkOdd,
      printDivisibleBy8,
      findMaxMatches
    ) where
import System.Random (randomRIO)
import Data.List (maximumBy)
----------------------------------------------------------------------------
-- Gera números aleatórios
----------------------------------------------------------------------------

generateRandomNumber :: Int -> Int -> IO Int 
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
-- Calcula o total de palitos na matriz (soma decimal de todos os '1')
----------------------------------------------------------------------------
calcTotalMatches :: [[Int]] -> Int
calcTotalMatches rowsList = sum (map sum rowsList)

----------------------------------------------------------------------------
-- Imprime se o total de palitos é divisível por 8
----------------------------------------------------------------------------
printDivisibleBy8 :: [[Int]] -> IO ()
printDivisibleBy8 rowsList = do
    let total = calcTotalMatches rowsList
    if total `mod` 8 == 0
        then putStrLn $ "O total de " ++ show total ++ " palitos é divisível por 8. (Posição perdedora)"
        else putStrLn $ "O total de " ++ show total ++ " palitos não é divisível por 8. (Posição vencedora)"

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

----------------------------------------------------------------------------
-- Converte um número decimal para sua representação binária
----------------------------------------------------------------------------
decimalToBinary :: Int -> [Int]
decimalToBinary 0 = [0]
decimalToBinary n = reverse (go n)
  where
    go 0 = []
    go n = let (q, r) = n `divMod` 2 in r : go q

----------------------------------------------------------------------------
-- Converte um número binário (lista) para decimal (inteiro)
----------------------------------------------------------------------------
binaryToDecimal :: [Int] -> Int
binaryToDecimal = foldl (\acc x -> acc * 2 + x) 0

----------------------------------------------------------------------------
-- Percorre uma fileira e retorna o número binário de palitinhos
----------------------------------------------------------------------------
processRowToBinary :: [Int] -> IO [Int]
processRowToBinary row = do

    let availableMatches = length (filter (== 1) row)
    let binaryMatches = decimalToBinary availableMatches

    return binaryMatches

----------------------------------------------------------------------------
-- Completa lista de números binários, acrescentando 0s à esquerda
----------------------------------------------------------------------------
normalizeRows :: [[Int]] -> [[Int]]
normalizeRows rowsList = map padWithZeros rowsList
  where
    padWithZeros row = replicate (3 - length row) 0 ++ row

----------------------------------------------------------------------------
-- Percorre a lista transposta e retorna os algarismos da soma decimal
----------------------------------------------------------------------------
transposeAndSum :: [[Int]] -> [Int]
transposeAndSum rowsList = map sum (transpose rowsList)
  where
    transpose [] = []
    transpose ([] : _) = []
    transpose xs = map head xs : transpose (map tail xs)

----------------------------------------------------------------------------
-- Checa se cada algarismo é ímpar ou não
----------------------------------------------------------------------------
checkOdd :: [Int] -> [Int]
checkOdd = map (\x -> if odd x then 1 else 0)

findBestOption :: [[Int]] -> [Int] -> (Int, Bool)
findBestOption binaryRowsList changeBitsRef = go binaryRowsList 0
  where
    go [] _ = (-1, False)                    -- Caso não encontre nenhuma lista correspondente, retorna (-1, False)
    go (x:xs) idx
      | match x changeBitsRef = (idx, True)  -- Se encontrar uma lista correspondente, retorna o índice e True
      | otherwise = go xs (idx + 1)          -- Continua a busca nas próximas listas

    -- Função que verifica se os dígitos 1 de x coincidem com os dígitos 1 de changeBitsRef
    match x changeBitsRef = all (\(a, b) -> b == 0 || a == 1) (zip x changeBitsRef)


findMaxMatches :: [[Int]] -> (Int, Int)
findMaxMatches rows = maximumBy (\(_, a) (_, b) -> compare a b) (zip [1..] (map (sum . filter (==1)) rows))