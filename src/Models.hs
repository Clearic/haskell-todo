module Models where

import Data.Bool (bool)

data TodoItem = TodoItem { complited :: Bool, title :: String } deriving (Eq)

instance Show TodoItem where
    show (TodoItem { complited = c, title = t }) = 
        '[':(bool ' ' 'x' c):']':' ':t

readTodoItem :: String -> Maybe TodoItem
readTodoItem ('[':' ':']':' ':str) = Just $ TodoItem False str
readTodoItem ('[':'x':']':' ':str) = Just $ TodoItem True str
readTodoItem _ = Nothing

data Error = IndexOutOfRange deriving (Show, Eq)

data Action = 
    Add String | 
    Remove Int | 
    Complite Int |
    Uncomplite Int deriving (Show, Eq)

data Command = 
    Exit | 
    Help |
    TodoAction Action deriving (Show, Eq)