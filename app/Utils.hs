module Utils (emptyRow, endGame, printGame) where

-- Utility functions for Jogo dos Palitinhos
vez = [0,1]

-- removerPalitos :: [Int] -> Int -> [Int]

emptyRow :: [Int] -> Bool
emptyRow row = null row

endGame :: [[Int]] -> Bool
endGame rowsList = null rowsList

printGame :: [[Int]] -> IO ()
printGame rowsList = mapM_ (\row -> do
    mapM_ (\matchstick -> 
        if matchstick == 1
        then putStr " |" 
        else return ()
      ) row
    putStrLn ""
  ) rowsList

-- lerEscolha :: IO Int