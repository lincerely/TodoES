module Todo where

import           Data.List
import           Text.Printf

data TodoState = Pending
               | InProgress
               | Done
               deriving (Eq, Show, Read, Ord, Enum)

data Todo = Todo { getIndex   :: Int
                 , getContent :: String
                 , getState   :: TodoState
                 } deriving (Eq, Show, Read, Ord)

data Command = CmdAdd String
             | CmdDelete Int
             | CmdUpdate Int TodoState
             | CmdUndo Action
             deriving (Eq, Show, Read)

issue :: Command -> [Todo] -> Either String Action
issue (CmdAdd newContent) [] = Right $ Add (Todo 0 newContent Pending)
issue (CmdAdd newContent) todos =
    Right $ Add (Todo (getIndex (last todos) + 1) newContent Pending)
issue (CmdDelete targetID) todos =
    let targetTodo = find (\t -> getIndex t == targetID) todos
    in  case targetTodo of
            Nothing -> Left $ printf "todo %d not found" targetID
            Just t  -> Right $ Delete t
issue (CmdUpdate targetID newState) todos =
    let targetTodo = find (\t -> getIndex t == targetID) todos
    in  case targetTodo of
            Nothing -> Left $ printf "todo %d not found" targetID
            Just (Todo _ _ oldState) ->
                Right $ UpdateState targetID oldState newState
issue (CmdUndo action) _ = Right $ Undo action

data Action = Add Todo
            | Delete Todo
            | UpdateState Int TodoState TodoState
            | Undo Action
            deriving (Eq, Show, Read)

apply :: Action -> [Todo] -> [Todo]
apply (Add    t                    ) = (++ [t])
apply (Delete t                    ) = delete t
apply (UpdateState todoID oldS newS) = updateTodoState todoID oldS newS
apply (Undo action                 ) = apply $ undo action

constructList :: [Action] -> [Todo]
constructList = foldr apply []

updateTodoState :: Int -> TodoState -> TodoState -> [Todo] -> [Todo]
updateTodoState _ _ _ [] = []
updateTodoState todoID oldState newState (t : todos)
    | todoID == i = Todo i c newState : todos
    | otherwise   = t : updateTodoState todoID oldState newState todos
    where (Todo i c _) = t

undo :: Action -> Action
undo (Add    t                ) = Delete t
undo (Delete t                ) = Add t
undo (UpdateState todoID s1 s2) = UpdateState todoID s2 s1
undo (Undo act                ) = act
