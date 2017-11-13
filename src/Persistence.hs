module Persistence where

import Data.List (intercalate)
import System.Directory (doesFileExist)
import Models

parseTodos :: String -> Maybe [TodoItem]
parseTodos str = traverse readTodoItem $ lines str

todosToStr :: [TodoItem] -> String
todosToStr todos = intercalate "\n" $ map show todos

-- loadTodos :: IO (Maybe [TodoItem])
-- loadTodos = fmap parseTodos $ readFile "todos.txt"

loadTodos :: IO (Maybe [TodoItem])
loadTodos = do 
    fileExists <- doesFileExist "todos.txt"
    if fileExists then 
        fmap parseTodos $ readFile "todos.txt"
    else
        return Nothing


saveTodos :: [TodoItem] -> IO ()
saveTodos todos = writeFile "todos.txt" $ todosToStr todos
