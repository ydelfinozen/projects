module CSVParser (loadCSV, saveCSV) where

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import DataTypes (Row(..))
import qualified Data.Vector as V

loadCSV :: FilePath -> IO (Either String [Row])
loadCSV filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> return $ Left err
    Right (_, rows) -> return $ Right (V.toList rows)

saveCSV :: FilePath -> [Row] -> IO ()
saveCSV filePath rows = do
  let csvData = encodeDefaultOrderedByName rows
  BL.writeFile filePath csvData
