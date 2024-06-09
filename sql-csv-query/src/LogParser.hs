module LogParser (loadLog) where

import Data.Text (Text, pack, unpack)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import DataTypes (Row(..))
import qualified Data.HashMap.Strict as HM

loadLog :: FilePath -> IO (Either String [Row])
loadLog filePath = do
  logData <- TIO.readFile filePath
  let rows = map parseLogLine (T.lines logData)
  return $ Right rows

parseLogLine :: Text -> Row
parseLogLine line = Row $ HM.fromList [(pack "LogEntry", line)]
