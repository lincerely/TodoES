module Main where

import           Control.Monad
import           Data.Maybe
import           System.Directory
import           System.IO
import           Text.Read
import           Todo

main :: IO()
main = do
    contents <- load "todo.txt"
    (tempName, tempHandle) <- openTempFile "." "temp"
    let maybeTodos = readMaybe contents :: Maybe [Action]
    todos' <- readCmd (fromMaybe [] maybeTodos)
    hPutStr tempHandle (show todos')
    hClose tempHandle
    isReplace <- doesFileExist "todo.txt"
    when isReplace $ removeFile "todo.txt"
    renameFile tempName "todo.txt"
    putStrLn "Bye."

load :: FilePath -> IO String
load path = do
    exists <- doesFileExist path
    if exists then readFile path
              else return ""

getList :: [Action] -> [Todo]
getList = foldr apply []

readCmd :: [Action] -> IO [Action]
readCmd actions = do
    mapM_ (putStrLn . formatTodo) $ getList actions
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
     "exit"      -> return actions
     _ -> do
         putStrLn "invalid command, expect [add/del/update/exit]"
         readCmd actions

readAdd :: [Action] -> [String] -> IO [Action]
readAdd actions [] = do
    putStrLn "add: invalid parameter, expect: content"
    return actions
readAdd actions input=
      case issue (CmdAdd (unwords input)) (getList actions) of
        Left err -> do
            putStrLn err
            return actions
        Right action -> return $ actions ++ [action]

readDel :: [Action] -> [String] -> IO [Action]
readDel actions [inputID]=
    let todoID = read inputID :: Int
     in case issue (CmdDelete todoID) (getList actions) of
          Left err -> do
              putStrLn err
              return actions
          Right action -> return $ actions ++  [action]
readDel actions _ = do
    putStrLn "del: invalid parameters, expect: todoID"
    return actions

readUpdate :: [Action] -> [String] -> IO [Action]
readUpdate actions [inputID,inputState] =
    let todoID = read inputID :: Int
        state' = read inputState :: TodoState
     in case issue (CmdUpdate todoID state') (getList actions) of
          Left err -> do
              putStrLn err
              return actions
          Right action -> return $ actions ++ [action]
readUpdate actions _ = do
    putStrLn "update: invalid parameters, expect: todoID state"
    return actions

formatTodo :: Todo -> String
formatTodo t = show (idx t) ++ " | " ++ content t ++ " | " ++ show (state t)
