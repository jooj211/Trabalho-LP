module Utils
    ( emptyRow,
      endGame,
      printGame,
      makeChoice,
      getValidRow,
      getValidMatches,
      removeMatches
    ) where

import Text.Read (readMaybe)

----------------------------------------------------------------------------
-- Utility functions for Jogo dos Palitinhos
----------------------------------------------------------------------------

-- Indica a vez de jogar (1 = jogador, -1 = computador)
vez :: Int
vez = 1

----------------------------------------------------------------------------
-- Check if a row is empty (all zeros)
----------------------------------------------------------------------------
emptyRow :: [Int] -> Bool
emptyRow row = all (== 0) row

----------------------------------------------------------------------------
-- Check if there are no rows left -> end of game
----------------------------------------------------------------------------
endGame :: [[Int]] -> Bool
endGame rowsList = null rowsList

----------------------------------------------------------------------------
-- Print the game: each `1` is shown as "|"
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
-- Make a choice: either the computer or the player chooses row and matches
----------------------------------------------------------------------------
makeChoice :: [[Int]] -> Int -> Int -> Int -> IO [[Int]]
makeChoice rowsList rowNumber matchsticks playerTurn = do
    if playerTurn == 0
        then do
            putStrLn "Vez do computador..."
            -- TODO: Implement computer logic (getComputerChoice, etc.)
            return rowsList -- Temporary placeholder
        else do
            putStrLn "Sua vez de jogar..."

            -- 1) Validate the row choice
            chosenRow <- getValidRow rowsList

            -- 2) Count how many `1`s are in that row
            let availableMatches = length (filter (== 1) (rowsList !! (chosenRow - 1)))

            -- 3) Validate the number of matches to remove
            chosenM <- getValidMatches availableMatches

            -- 4) Remove matches and possibly delete the row if it's empty
            let updatedRows = removeMatches rowsList chosenRow chosenM

            return updatedRows

----------------------------------------------------------------------------
-- Ask the user for a valid row
--   rowNumber here is the total number of rows that can be chosen
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
-- Ask the user for a valid number of matches to remove
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

----------------------------------------------------------------------------
-- Remove matches from a row (chosenRow) and update the rowsList accordingly
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
-- Remove the last 'm' matches (i.e., convert up to m of the '1's to '0's)
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
-- Helper: replace a row at the specified index
----------------------------------------------------------------------------
replaceAt :: Int -> [Int] -> [[Int]] -> [[Int]]
replaceAt idx newRow rows =
    take idx rows ++ [newRow] ++ drop (idx + 1) rows

----------------------------------------------------------------------------
-- Helper: delete a row at the specified index
----------------------------------------------------------------------------
deleteRowAt :: Int -> [[Int]] -> [[Int]]
deleteRowAt idx rows =
    take idx rows ++ drop (idx + 1) rows
