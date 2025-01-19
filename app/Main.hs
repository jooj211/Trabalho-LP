----------------------------------------------------------------------------
----- Trabalho Prático 1 - Linguagens de Programação (DCC019 - 2024.2) -----
----------------------------- Desenvolvido por: ----------------------------
--               Jonatas Dias Machado Costa (202165077AC)
--              Maria Luísa Riolino Guimarães (202165563C)
----------------------------------------------------------------------------

import Text.Read 
import Control.Monad (replicateM)

import Utils
import GameLogic

----------------------------------------------------------------------------
-- Menu inicial do jogo
----------------------------------------------------------------------------

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

----------------------------------------------------------------------------
-- Gera estado inicial do jogo (número aleatório de palitos por linha)
--   n = número de linhas
----------------------------------------------------------------------------

initialState :: Int -> IO [[Int]]
initialState n = replicateM n generateRandomMatches

----------------------------------------------------------------------------
-- Loop do jogo e verificação de vitória/derrota

--   rowsList = lista com as fileiras de palitinhos restantes
--   playerTurn = indica de quem é o turno [1 = usuário, -1 = computador]
--   gameMode = indica o modo do jogo [1 = fácil, 2 = difícil]
----------------------------------------------------------------------------

gameLoop :: [[Int]] -> Int -> Int -> IO ()
gameLoop rowsList playerTurn gameMode = do
    if endGame rowsList
        then putStrLn $ if playerTurn == 1 then "VOCÊ PERDEU!" else "VOCÊ VENCEU!"
        else do
            putStrLn "Estado atual do jogo:"
            printGame rowsList

            -- Chama a função makeChoice, que trata tanto a jogada do computador quanto a do jogador
            updatedRows <- makeChoice rowsList playerTurn gameMode

            -- Alterna para o próximo turno (se for 1 vai para -1 e vice-versa)
            gameLoop updatedRows (if playerTurn == 1 then (-1) else 1) gameMode

----------------------------------------------------------------------------
-- Escopo principal do jogo
----------------------------------------------------------------------------

main :: IO ()
main = do
  numberOfRows <- generateRandomRows

  putStrLn "BOAS-VINDAS AO JOGO DOS PALITINHOS!"
  menuOption <- menu

  putStrLn $ "Você escolheu a opção: " ++ show menuOption
  case menuOption of
    0 -> putStrLn "Saindo do jogo. Até logo!"
    1 -> do
        putStrLn "Iniciando o nível Fácil..."
        rowsList <- initialState numberOfRows
        putStrLn $ "Número de linhas geradas = " ++ show numberOfRows
        putStrLn ""
        gameLoop rowsList 1 1
    2 -> do
        putStrLn "Iniciando o nível Difícil..."
        rowsList <- initialState numberOfRows
        putStrLn $ "Número de linhas geradas = " ++ show numberOfRows
        putStrLn ""
        gameLoop rowsList (-1) (2)
    _ -> return ()