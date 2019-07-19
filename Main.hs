module Main where

import           Control.Monad
import           Data.Maybe
import           System.Directory
import           System.IO
import           Text.Read
import           Todo

main :: IO()
main = do
    contents <- load "save.txt"
    (tempName, tempHandle) <- openTempFile "." "temp"
    let maybeTodos = readMaybe contents :: Maybe [Action]
    todos' <- readCmd (fromMaybe [] maybeTodos)
    hPutStr tempHandle (show todos')
    hClose tempHandle
    isReplace <- doesFileExist "save.txt"
    when isReplace $ removeFile "save.txt"
    renameFile tempName "save.txt"
    putStrLn "Bye."

load :: FilePath -> IO String
load path = do
    exists <- doesFileExist path
    if exists then readFile path
              else return ""

readCmd :: [Action] -> IO [Action]
readCmd actions = do
    mapM_ (putStrLn . formatTodo) $ constructList actions
    putStr "> "
    input <- getLine
    let (command:args) = words input
     in case command of
     "add"    -> do
         actions' <- readAdd actions args
         readCmd actions'
     "del"    -> do
         actions' <- readDel actions args
         readCmd actions'
     "update" -> do
         actions' <- readUpdate actions args
         readCmd actions'
     "undo" -> do
         actions' <- readUndo actions args
         readCmd actions'
     "stack" -> do
         mapM_ print actions
         readCmd actions
     "exit"      -> return actions
     _ -> do
         putStrLn "invalid command, expect [add/del/update/undo/exit]"
         readCmd actions

readUndo :: [Action] -> [String] -> IO [Action]
readUndo (prevAction:actions) [] =
      case issue (CmdUndo prevAction) (constructList actions) of
        Left err -> do
            putStrLn err
            return actions
        Right action -> return $ action : prevAction : actions
readUndo actions _ = do
    putStrLn "undo: no parameter is needed"
    return actions

readAdd :: [Action] -> [String] -> IO [Action]
readAdd actions [] = do
    putStrLn "add: invalid parameter, expect: content"
    return actions
readAdd actions input=
      case issue (CmdAdd (unwords input)) (constructList actions) of
        Left err -> do
            putStrLn err
            return actions
        Right action -> return $ action : actions

readDel :: [Action] -> [String] -> IO [Action]
readDel actions [inputID]=
    let todoID = read inputID :: Int
     in case issue (CmdDelete todoID) (constructList actions) of
          Left err -> do
              putStrLn err
              return actions
          Right action -> return $ action : actions
readDel actions _ = do
    putStrLn "del: invalid parameters, expect: todoID"
    return actions

readUpdate :: [Action] -> [String] -> IO [Action]
readUpdate actions [inputID,inputState] =
    let todoID = read inputID :: Int
        state' = read inputState :: TodoState
     in case issue (CmdUpdate todoID state') (constructList actions) of
          Left err -> do
              putStrLn err
              return actions
          Right action -> return $ action : actions
readUpdate actions _ = do
    putStrLn "update: invalid parameters, expect: todoID state"
    return actions

formatTodo :: Todo -> String
formatTodo t = show (idx t) ++ " | " ++ content t ++ " | " ++ show (state t)
