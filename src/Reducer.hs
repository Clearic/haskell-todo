module Reducer where

import Models

add :: [TodoItem] -> String -> [TodoItem]
add todos str = todos ++ [TodoItem False str]

remove :: [TodoItem] -> Int -> [TodoItem]
remove [] _ = []
remove (_:ts) 1 = ts
remove (t:ts) index
    | index < 1 = t:ts
    | otherwise = t:remove ts (index-1)

isInRange :: [a] -> Int -> Bool
isInRange list index
    | index <= 0 = False
    | index > length list = False
    | otherwise = True

removeEither :: [TodoItem] -> Int -> Either Error [TodoItem]
removeEither list index
    | isInRange list index = Right $ remove list index
    | otherwise = Left IndexOutOfRange
    
complite :: [TodoItem] -> Int -> [TodoItem]
complite [] _ = []
complite (t:ts) 1 = (TodoItem True (title t)):ts
complite (t:ts) index 
    | index < 1 = t:ts
    | otherwise = t:complite ts (index-1)

compliteEither :: [TodoItem] -> Int -> Either Error [TodoItem]
compliteEither list index
    | isInRange list index = Right $ complite list index
    | otherwise = Left IndexOutOfRange

uncomplite :: [TodoItem] -> Int -> [TodoItem]
uncomplite [] _ = []
uncomplite (t:ts) 1 = (TodoItem False (title t)):ts
uncomplite (t:ts) index 
    | index < 1 = t:ts
    | otherwise = t:uncomplite ts (index-1)

uncompliteEither :: [TodoItem] -> Int -> Either Error [TodoItem]
uncompliteEither list index
    | isInRange list index = Right $ uncomplite list index 
    | otherwise = Left IndexOutOfRange

reducer :: [TodoItem] -> Action -> Either Error [TodoItem]
reducer todos (Add str) = Right $ add todos str
reducer todos (Remove index) = removeEither todos index
reducer todos (Complite index) = compliteEither todos index
reducer todos (Uncomplite index) = uncompliteEither todos index
