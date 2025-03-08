----------------------------------------------------------------------------
----- Trabalho Prático 1 - Linguagens de Programação (DCC019 - 2024.2) -----
----------------------------- Desenvolvido por: ----------------------------
--               Jonatas Dias Machado Costa (202165077AC)
--              Maria Luísa Riolino Guimarães (202165563C)
----------------------------------------------------------------------------

module GameLogic
    ( makeChoice,
      getValidRow,
      getValidMatches
    ) where

import Utils
import ComputerAI
import Text.Read (readMaybe)

-------------------------------------------------------------------------------
-- O computador ou o jogador escolhem uma fileira e um número de palitinhos
--   rowsList = situação atual do jogo
--   playerTurn = turno atual (1 = jogador; -1 = computador)
--   gameMode = modo de jogo (1 = fácil; 2 = difícil)
-------------------------------------------------------------------------------
makeChoice :: [[Int]] -> Int -> Int -> IO [[Int]]
makeChoice rowsList playerTurn gameMode = do
    printDivisibleBy8 rowsList
    if playerTurn == -1
        then do
            putStrLn "Vez do computador..."
            
            updatedRows <- computerMove rowsList gameMode

            return updatedRows    
        else do
            putStrLn "Sua vez de jogar..."

            chosenRow <- getValidRow rowsList
            let availableMatches = length (filter (== 1) (rowsList !! (chosenRow - 1)))
            chosenM <- getValidMatches availableMatches
            let updatedRows = removeMatches rowsList chosenRow chosenM

            return updatedRows


----------------------------------------------------------------------------
-- Recebe e valida fileira escolhida pelo usuário
--   rowNumber = número total de fileiras que podem ser escolhidas
----------------------------------------------------------------------------
getValidRow :: [[Int]] -> IO Int
getValidRow rowsList = do
    let rowNumber = length rowsList
    putStrLn $ "Escolha uma linha (1 a " ++ show rowNumber ++ "):"
    input <- getLine
    case readMaybe input of
        Just r ->
            if r `elem` [1 .. rowNumber]
                then return r
                else do
                    putStrLn "Linha inválida! Tente novamente."
                    getValidRow rowsList
        Nothing -> do
            putStrLn "Entrada inválida! Tente novamente."
            getValidRow rowsList

----------------------------------------------------------------------------
-- Recebe e valida número de palitinhos removidos pelo usuário
----------------------------------------------------------------------------
getValidMatches :: Int -> IO Int
getValidMatches matchsticks = do
    putStrLn $ "Escolha o número de palitos (1 a " ++ show matchsticks ++ "):"
    input <- getLine
    case readMaybe input of
        Nothing -> do
            putStrLn "Valor inválido! Tente novamente."
            getValidMatches matchsticks
        Just m ->
            if m `elem` [1 .. matchsticks]
                then return m
                else do
                    putStrLn "Quantidade fora do intervalo. Tente novamente."
                    getValidMatches matchsticks
