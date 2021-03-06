module Main where

import Control.Monad.Trans
import System.Console.Haskeline
import Text.Megaparsec.Error

import Parser

process :: String -> IO ()
process line = do
  let res = parseTopLevel line
  case res of
    Left err -> putStr $ parseErrorPretty err
    Right ex -> print ex

main :: IO ()
main = runInputT defaultSettings loop
    where
      loop = do
        minput <- getInputLine "calc> "
        case minput of
          Nothing -> outputStrLn "Goodbye."
          Just input -> liftIO (process input) >> loop
