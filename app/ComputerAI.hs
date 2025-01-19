----------------------------------------------------------------------------
----- Trabalho Prático 1 - Linguagens de Programação (DCC019 - 2024.2) -----
----------------------------- Desenvolvido por: ----------------------------
--               Jonatas Dias Machado Costa (202165077AC)
--              Maria Luísa Riolino Guimarães (202165563C)
----------------------------------------------------------------------------

module ComputerAI
    ( easyMode,
      hardMode,
      computerMove
    ) where

import Utils

-------------------------------------------------------------------------------
-- Executa a jogada do computador, de acordo com o modo de jogo escolhido
-------------------------------------------------------------------------------
computerMove :: [[Int]] -> Int -> IO [[Int]]
computerMove rowsList gameMode
    | gameMode == 1 = easyMode rowsList  -- Modo fácil
    | gameMode == 2 = hardMode rowsList  -- Modo difícil
    | otherwise = error "Modo de jogo inválido"

-------------------------------------------------------------------------------
-- Modo fácil: fileira e número de palitinhos escolhidos aleatoriamente
-------------------------------------------------------------------------------
easyMode :: [[Int]] -> IO [[Int]]
easyMode rowsList = do
    let numValidRows = getAvailableRows rowsList

    -- Escolhe linha aleatória
    chosenRowIndex <- generateRandomNumber 0 (numValidRows - 1)

    -- Escolhe número aleatório de palitos a serem removidos
    let availableMatches = length (filter (== 1) (rowsList !! (chosenRowIndex)))
    chosenMatches <- generateRandomNumber 1 availableMatches
    putStrLn $ "Removendo " ++ show chosenMatches ++ " palitinho(s) da linha " ++ show (chosenRowIndex + 1)

    let updatedRows = removeMatches rowsList (chosenRowIndex+1) chosenMatches

    return updatedRows 

-------------------------------------------------------------------------------
-- Modo difícil: computador executa estratégia vencedora
--   - A quantidade de palitos em cada fileira é representada na base 2
--   - A soma decimal da representação é executada
--   - Após a remoção dos palitos, a soma decimal das representações binárias
--     deve conter todos os algarismos pares
-------------------------------------------------------------------------------

hardMode :: [[Int]] -> IO [[Int]]
hardMode rowsList = do
    -- Converte cada fileira para binário
    binaryRows <- mapM processRowToBinary rowsList

    -- Normaliza as fileiras
    let normBinaryRows = normalizeRows binaryRows

    -- putStrLn $ "Linhas normalizadas (binário): " ++ show normBinaryRows

    -- Calcula a soma decimal das colunas
    let columnSums = transposeAndSum normBinaryRows

    -- putStrLn $ "Soma decimal das colunas: " ++ show columnSums

    let changeBitAt = checkOdd columnSums

    -- putStrLn $ "Alterar bits em: " ++ show changeBitAt
    
    let (idx, flag) = findBestOption normBinaryRows changeBitAt
    -- putStrLn $ "Índice encontrado: " ++ show idx
    -- putStrLn $ "Correspondência encontrada? " ++ show flag

    if flag && (changeBitAt /= [0,0,0]) then do
        let prevNumberOfMatches = binaryToDecimal (normBinaryRows !! idx)

        -- Inverte os números 1 para 0 na linha correspondente ao índice, quando há correspondência com changeBitAt
        let invertedRow = zipWith (\bit change -> if change == 1 && bit == 1 then 0 else bit) (normBinaryRows !! idx) changeBitAt
        
        -- Calcula o valor decimal da linha invertida
        let newNumberOfMatches = binaryToDecimal invertedRow
        
        -- Chama a função de remoção de palitos (substitua `removeMatches` pela função correspondente)
        let updatedRows = removeMatches rowsList (idx+1) (prevNumberOfMatches - newNumberOfMatches)
        putStrLn $ "Removendo " ++ show (prevNumberOfMatches - newNumberOfMatches) ++ " palitinho(s) da linha " ++ show (idx + 1)

        -- Retorna a lista de linhas normalizadas
        return updatedRows
    else do
        -- Encontrar a fileira com o maior número de palitos
        let (maxIdx, maxMatches) = findMaxMatches rowsList
        
        -- Remover um palito dessa fileira
        let updatedRows = removeMatches rowsList (maxIdx) 1
        putStrLn $ "Removendo um palitinho da linha " ++ show maxIdx  -- Mostra o índice da linha com mais palitos

        -- Retorna a lista de linhas após a remoção
        return updatedRows