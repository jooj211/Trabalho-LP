----------------------------------------------------------------------------
----- Trabalho Prático 1 - Linguagens de Programação (DCC019 - 2024.2) -----
----------------------------- Desenvolvido por: ----------------------------
--               Jonatas Dias Machado Costa (202165077AC)
--              Maria Luísa Riolino Guimarães (202165563C)
----------------------------------------------------------------------------

module ComputerAI
    ( easyMode,
    --   hardMode,
      computerMove
    ) where

import Utils

computerMove :: [[Int]] -> Int -> IO [[Int]]
computerMove rowsList gameMode
    | gameMode == 1 = easyMode rowsList  -- Modo fácil
    -- | gameMode == 2 = hardMode rowsList  -- Modo difícil
    | otherwise = error "Modo de jogo inválido"

easyMode :: [[Int]] -> IO [[Int]]
easyMode rowsList = do
    let numValidRows = getAvailableRows rowsList

    -- Escolhe linha aleatória
    chosenRowIndex <- generateRandomNumber 0 (numValidRows - 1)
    let chosenRow = rowsList !! chosenRowIndex

    -- Escolhe número aleatório de palitos a serem removidos
    let availableMatches = length (filter (== 1) chosenRow)
    chosenMatches <- generateRandomNumber 1 availableMatches
    putStrLn $ "Removendo " ++ show chosenMatches ++ " palitinho(s) da linha " ++ show (chosenRowIndex + 1)

    let updatedRows = removeMatches rowsList chosenRowIndex chosenMatches

    return updatedRows 


-- hardMode :: [[Int]] -> [[Int]]