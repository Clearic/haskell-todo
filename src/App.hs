module App where

import Models
import Reducer
import CommandParser
import Persistence

printTodo :: (Int, TodoItem) -> IO ()
printTodo (i, todo) = putStrLn $ show i ++ ") " ++ show todo

printTodos :: [TodoItem] -> IO ()
printTodos todos = do
    mapM_ printTodo $ zip [1..] todos

printHelp :: IO ()
printHelp = do
    putStrLn "help - Print help"
    putStrLn "add <title> - Add todo item"
    putStrLn "remove <index> - Remove todo item at index"
    putStrLn "complite <index> - Make todo item as complited at index"
    putStrLn "uncomplite <index> - Make todo item as uncomplited at index"
    putStrLn "exit - Exit"
    return ()

printError :: Error -> IO ()
printError IndexOutOfRange = putStrLn "Index out of range"

loop :: [TodoItem] -> IO ()
loop todos = do
    printTodos todos
    line <- getLine
    case parseCommand line of
        Nothing -> putStrLn "Invalid Command" >> loop todos
        Just Exit -> return ()
        Just Help -> printHelp >> loop todos
        Just (TodoAction act) -> 
            case reducer todos act of
                Left err -> printError err >> loop todos
                Right newTodos -> (saveTodos newTodos) >> loop newTodos

initTodos :: [TodoItem]
initTodos = [TodoItem False "Test 1", TodoItem False "Test 2", TodoItem False "Test 3"]

run :: IO ()
run = do
    maybeTodos <- loadTodos
    case maybeTodos of
        Just todos -> loop todos
        Nothing -> loop initTodos
