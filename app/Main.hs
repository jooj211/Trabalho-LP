{-
TODO:
- Gerar um número aleatório entre 2 e 1000
- Criar um vetor de vetores de tamanho igual ao número gerado
- Preencher cada vetor com número aleatório de palitos entre [1,3,5,7]

-}

import System.Random (mkStdGen, randomR, randomRIO)
import Text.Read
import Control.Monad (replicateM)
import Utils



-- Function to generate a random number between 1 and 10
generateRandomNumber :: Int -> Int -> IO Int --
generateRandomNumber minNumber maxNumber = randomRIO(minNumber, maxNumber)

generateRandomRows :: IO Int -- Gera um número aleatório entre 2 e 1000
generateRandomRows = generateRandomNumber 2 10


generateRandomMatches :: IO [Int]
generateRandomMatches = do
  num <- generateRandomNumber 1 4 -- Garante que `num * 2 - 1` esteja no intervalo [1, 7]
  let n = num * 2 - 1
  return $ replicate n 1 ++ replicate (7 - n) 0

menu :: IO Int
menu = do
  putStrLn "Escolha um nível de jogo:"
  putStrLn "[0] Sair do jogo"
  putStrLn "[1] Fácil"
  putStrLn "[2] Difícil"

  gameLevel <- getLine

  case readMaybe gameLevel :: Maybe Int of
    Just n | n `elem` [0, 1, 2] -> return n
    _ -> do
      putStrLn "Entrada inválida! Tente novamente."
      menu


-- estadoInicial :: Número de Linhas -> Primeiro jogador -> Conjunto de filas
initialState :: Int -> IO [[Int]]
initialState n = replicateM n generateRandomMatches
    
-- loopJogo :: [[Int]] -> Int -> [[Int]]


-- Escopo principal do programa
main :: IO ()
main = do
  numberOfRows <- generateRandomRows

  putStrLn "BOAS-VINDAS AO JOGO DOS PALITINHOS!"
  choice <- menu

  putStrLn $ "Você escolheu a opção: " ++ show choice
  case choice of
    0 -> putStrLn "Saindo do jogo. Até logo!"
    1 -> putStrLn "Iniciando o nível Fácil..."
    2 -> putStrLn "Iniciando o nível Difícil..."
    _ -> return ()

  rowsList <- initialState numberOfRows
  
  putStrLn $ "Número de linhas geradas = " ++ show numberOfRows
  putStrLn $ ""
  printGame rowsList

