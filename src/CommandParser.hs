module CommandParser where

import Text.Read (readMaybe)
import Models

parseCommand :: String -> Maybe Command
parseCommand "exit" = Just Exit
parseCommand "help" = Just Help
parseCommand ('a':'d':'d':' ':str) = Just $ TodoAction $ Add str
parseCommand ('r':'e':'m':'o':'v':'e':' ':str) =
    fmap (TodoAction . Remove) (readMaybe str)
parseCommand ('c':'o':'m':'p':'l':'i':'t':'e':' ':str) =
    fmap (TodoAction . Complite) (readMaybe str)
parseCommand ('u':'n':'c':'o':'m':'p':'l':'i':'t':'e':' ':str) =
    fmap (TodoAction . Uncomplite) (readMaybe str)
parseCommand _ = Nothing
