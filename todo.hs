module Todo where

import           Data.List

main :: IO()
main = do
    todos <- readCmd []
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
     let content = read ("\"" ++ contentInput ++ "\""):: String
      in case issue (CmdAdd content) todos of
           Left err -> do
               print err
               return todos
           Right action -> return $ apply action todos

readDel :: [Todo] -> IO [Todo]
readDel todos = do
    print "Todo ID? [int]"
    idxInput <- getLine
    let idx = read idxInput :: Int
     in case issue (CmdDelete idx) todos of
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
    let idx = read idxInput :: Int
        state = read stateInput :: TodoState
     in case issue (CmdUpdate idx state) todos of
          Left err -> do
              print err
              return todos
          Right action -> return $ apply action todos

data TodoState = Pending
               | InProgress
               | Done
               deriving (Eq, Show, Read, Ord, Enum)

data Todo = Todo { idx     :: Int
                 , content :: String
                 , state   :: TodoState
                 } deriving (Eq, Show, Read, Ord)

data Command = CmdAdd String
             | CmdDelete Int
             | CmdUpdate Int TodoState
             deriving (Eq, Show, Read)

issue :: Command -> [Todo] -> Either String Action
issue (CmdAdd newContent) ts = Right $ Add (Todo (length ts) newContent Pending)
issue (CmdDelete targetID) ts =
    let targetTodo = find (\t -> idx t == targetID) ts
     in case targetTodo of
          Nothing -> Left $ "todo with id " ++ show targetID ++ " not found"
          Just t  -> Right $ Delete t
issue (CmdUpdate targetID newState) ts =
    let targetTodo = find (\t -> idx t == targetID) ts
     in case targetTodo of
          Nothing -> Left $ "todo with id " ++ show targetID ++ " not found"
          Just (Todo _ _ oldState) -> Right $ UpdateState targetID oldState newState

data Action = Add Todo
            | Delete Todo
            | UpdateState Int TodoState TodoState
            | Undo Action
            deriving (Eq, Show, Read)

apply :: Action -> [Todo] -> [Todo]
apply (Add t)                        = (++[t])
apply (Delete t)                     = delete t
apply (UpdateState todoID oldS newS) = updateTodoState todoID oldS newS
apply (Undo action)                  = apply $ undo action

updateTodoState :: Int -> TodoState -> TodoState -> [Todo] -> [Todo]
updateTodoState _ _ _ [] = []
updateTodoState todoID oldState newState  (t:ts)
  | todoID == i =  Todo i c newState : ts
  | otherwise = t : updateTodoState todoID oldState newState ts
  where (Todo i c _) = t

undo :: Action -> Action
undo (Add t)                    = Delete t
undo (Delete t)                 = Add t
undo (UpdateState todoID s1 s2) = UpdateState todoID s2 s1
undo (Undo act)                 = act
