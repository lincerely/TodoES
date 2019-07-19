module Main where

import           Todo

main :: IO()
main = do
    _ <- readCmd []
    putStrLn "Bye."

readCmd :: [Todo] -> IO [Todo]
readCmd todos = do
    mapM_ (putStrLn . formatTodo) todos
    putStr "> "
    input <- getLine
    let (command:args) = words input
     in case command of
     "add"    -> do
         todos' <- readAdd todos args
         readCmd todos'
     "del"    -> do
         todos' <- readDel todos args
         readCmd todos'
     "update" -> do
         todos' <- readUpdate todos args
         readCmd todos'
     "exit"      -> return todos
     _ -> do
         putStrLn "invalid command, expect [add/del/update/exit]"
         readCmd todos

readAdd :: [Todo] -> [String] -> IO [Todo]
readAdd todos [] = do
    putStrLn "add: invalid parameter, expect: content"
    return todos
readAdd todos input=
      case issue (CmdAdd (unwords input)) todos of
        Left err -> do
            putStrLn err
            return todos
        Right action -> return $ apply action todos

readDel :: [Todo] -> [String] -> IO [Todo]
readDel todos [inputID]=
    let todoID = read inputID :: Int
     in case issue (CmdDelete todoID) todos of
          Left err -> do
              putStrLn err
              return todos
          Right action -> return $ apply action todos
readDel todos _ = do
    putStrLn "del: invalid parameters, expect: todoID"
    return todos

readUpdate :: [Todo] -> [String] -> IO [Todo]
readUpdate todos [inputID,inputState] =
    let todoID = read inputID :: Int
        state' = read inputState :: TodoState
     in case issue (CmdUpdate todoID state') todos of
          Left err -> do
              putStrLn err
              return todos
          Right action -> return $ apply action todos
readUpdate todos _ = do
    putStrLn "update: invalid parameters, expect: todoID state"
    return todos

formatTodo :: Todo -> String
formatTodo t = show (idx t) ++ " | " ++ content t ++ " | " ++ show (state t)
