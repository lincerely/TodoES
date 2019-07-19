module Todo where

import           Data.List

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
             | CmdUndo Action
             deriving (Eq, Show, Read)

issue :: Command -> [Todo] -> Either String Action
issue (CmdAdd newContent) ts = Right $ Add (Todo (length ts) newContent Pending)
issue (CmdDelete targetID) ts =
    let targetTodo = find (\t -> idx t == targetID) ts
     in case targetTodo of
          Nothing -> 
              Left $ "todo with id " ++ show targetID ++ " not found"
          Just t  -> 
              Right $ Delete t
issue (CmdUpdate targetID newState) ts =
    let targetTodo = find (\t -> idx t == targetID) ts
     in case targetTodo of
          Nothing -> 
              Left $ "todo with id " ++ show targetID ++ " not found"
          Just (Todo _ _ oldState) -> 
              Right $ UpdateState targetID oldState newState
issue (CmdUndo action) _ = Right $ Undo action

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

constructList :: [Action] -> [Todo]
constructList = foldr apply []

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
