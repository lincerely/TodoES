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
     "add"    ->
         case readAdd actions args of
           Left err -> do
               print err
               readCmd actions
           Right actions' ->
               readCmd actions'
     "del"    ->
         case readDel actions args of
           Left err -> do
               print err
               readCmd actions
           Right actions' ->
               readCmd actions'
     "update" ->
         case readUpdate actions args of
           Left err -> do
               print err
               readCmd actions
           Right actions' ->
               readCmd actions'
     "undo" ->
         case readUndo actions args of
           Left err -> do
               print err
               readCmd actions
           Right actions' ->
               readCmd actions'
     "stack" -> do
         mapM_ print actions
         readCmd actions
     "exit"      -> return actions
     _ -> do
         putStrLn "invalid command, expect [add/del/update/undo/exit]"
         readCmd actions

actionMap :: [(String, [Action] -> [String] -> Either String [Action])]
actionMap = [("add", readAdd)
            ,("del", readDel)
            ,("update", readUpdate)
            ,("undo", readUndo)
            ]

readUndo :: [Action] -> [String] -> Either String [Action]
readUndo (prevAction:actions) [] =
    (:prevAction:actions) <$> issue (CmdUndo prevAction) (constructList actions)
readUndo _ _ = Left "undo: invalid parameter, expect: none"

readAdd :: [Action] -> [String] -> Either String [Action]
readAdd _ [] = Left "add: invalid parameter, expect: content"
readAdd actions input =
      (:actions) <$> issue (CmdAdd (unwords input)) (constructList actions)

readDel :: [Action] -> [String] -> Either String [Action]
readDel actions [inputID]=
    let todoID = read inputID :: Int
     in (:actions) <$> issue (CmdDelete todoID) (constructList actions)
readDel _ _ = Left "del: invalid parameters, expect: todoID"

readUpdate :: [Action] -> [String] -> Either String [Action]
readUpdate actions [inputID,inputState] =
    let todoID = read inputID :: Int
        state' = read inputState :: TodoState
     in (:actions) <$> issue (CmdUpdate todoID state') (constructList actions)
readUpdate _ _ = Left "update: invalid parameters, expect: todoID state"

formatTodo :: Todo -> String
formatTodo t = show (getIndex t) ++ " | " ++ getContent t ++ " | " ++ show (getState t)
