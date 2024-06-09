module SQLParser (parseQuery, Query(..)) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Text (Text, pack)

data Query = Query
  { selectFields :: [Text]
  , fromTable :: Text
  , whereCondition :: Maybe Text
  } deriving (Show, Eq)

type Parser = Parsec Void String

parseQuery :: String -> Either (ParseErrorBundle String Void) Query
parseQuery = parse queryParser ""

queryParser :: Parser Query
queryParser = do
  _ <- string "SELECT"
  _ <- space
  fields <- field `sepBy1` (char ',' >> space)
  _ <- space
  _ <- string "FROM"
  _ <- space
  table <- some alphaNumChar
  return $ Query (map pack fields) (pack table) Nothing

field :: Parser String
field = some alphaNumChar
