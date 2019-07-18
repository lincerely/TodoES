module Main where

import           Todo

main :: IO()
main = do
    _ <- readCmd []
    print "Bye."

readCmd :: [Todo] -> IO [Todo]
readCmd todos = do
    mapM_ print todos
    print "Command? [add/del/update]"
    input <- getLine
    let cmdStr = read ("\""++input++"\"") :: String
     in case cmdStr of
     "add"    -> do
         todos' <- readAdd todos
         readCmd todos'
     "del"    -> do
         todos' <- readDel todos
         readCmd todos'
     "update" -> do
         todos' <- readUpdate todos
         readCmd todos'
     _        -> return todos

readAdd :: [Todo] -> IO [Todo]
readAdd todos = do
     print "Content? [string]"
     contentInput <- getLine
     let contentStr = read ("\"" ++ contentInput ++ "\""):: String
      in case issue (CmdAdd contentStr) todos of
           Left err -> do
               print err
               return todos
           Right action -> return $ apply action todos

readDel :: [Todo] -> IO [Todo]
readDel todos = do
    print "Todo ID? [int]"
    idxInput <- getLine
    let todoID = read idxInput :: Int
     in case issue (CmdDelete todoID) todos of
          Left err -> do
              print err
              return todos
          Right action -> return $ apply action todos

readUpdate :: [Todo] -> IO [Todo]
readUpdate todos = do
    print "Todo ID? [int]"
    idxInput <- getLine
    print "Status? [Pending/InProgress/Done]"
    stateInput <- getLine
    let todoID = read idxInput :: Int
        state' = read stateInput :: TodoState
     in case issue (CmdUpdate todoID state') todos of
          Left err -> do
              print err
              return todos
          Right action -> return $ apply action todos
