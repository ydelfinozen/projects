module Main where

import System.IO
import CSVParser (loadCSV)
import LogParser (loadLog)
import QueryEngine (executeQuery)
import DataTypes (Row(..))
import SQLParser (parseQuery, Query)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.List (isSuffixOf)
import qualified Data.HashMap.Strict as HM
import Data.Text (unpack)

-- Global variable to store query results
queryResults :: IORef [Row]
queryResults = unsafePerformIO $ newIORef []

main :: IO ()
main = do
  putStrLn "Welcome to SQL-like Query on CSV and Log Files"
  commandLoop

commandLoop :: IO ()
commandLoop = do
  putStr "> "
  hFlush stdout
  command <- getLine
  case words command of
    ("load":filePath:_)  -> handleLoad filePath
    ("query":_)          -> handleQuery (unwords $ tail $ words command)
    ("save":filePath:_)  -> handleSave filePath
    ("help":_)           -> handleHelp
    _                    -> putStrLn "Unknown command"
  commandLoop

handleLoad :: FilePath -> IO ()
handleLoad filePath = do
  putStrLn $ "Loading file: " ++ filePath
  if ".csv" `isSuffixOf` filePath
    then loadCSVFile filePath
    else loadLogFile filePath

loadCSVFile :: FilePath -> IO ()
loadCSVFile filePath = do
  putStrLn "Attempting to load CSV file"
  result <- loadCSV filePath
  case result of
    Left err -> putStrLn $ "Error loading CSV: " ++ err
    Right rows -> do
      putStrLn $ "Loaded CSV with " ++ show (length rows) ++ " rows"
      writeIORef queryResults rows

loadLogFile :: FilePath -> IO ()
loadLogFile filePath = do
  putStrLn "Attempting to load log file"
  result <- loadLog filePath
  case result of
    Left err -> putStrLn $ "Error loading log file: " ++ err
    Right rows -> do
      putStrLn $ "Loaded log with " ++ show (length rows) ++ " rows"
      writeIORef queryResults rows

handleQuery :: String -> IO ()
handleQuery queryStr = do
  putStrLn $ "Parsing query: " ++ queryStr
  let parsedQuery = parseQuery queryStr
  case parsedQuery of
    Left err -> putStrLn $ "Error parsing query: " ++ show err
    Right query -> do
      putStrLn $ "Parsed query: " ++ show query
      rows <- readIORef queryResults
      putStrLn $ "Rows to be queried: " ++ show (length rows)
      let result = executeQuery query rows
      putStrLn $ "Query executed, " ++ show (length result) ++ " rows returned"
      writeIORef queryResults result

handleSave :: FilePath -> IO ()
handleSave filePath = do
  putStrLn $ "Saving results to: " ++ filePath
  rows <- readIORef queryResults
  let formattedRows = map formatRow rows
  writeFile filePath (unlines formattedRows)
  putStrLn "Results saved."

formatRow :: Row -> String
formatRow (Row hm) = unwords (map (unpack . snd) (HM.toList hm))

handleHelp :: IO ()
handleHelp = putStrLn "Usage: load <file> | query <SQL-like query> | save <file> | help"
