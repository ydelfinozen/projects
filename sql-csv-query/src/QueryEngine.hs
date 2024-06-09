module QueryEngine (executeQuery) where

import DataTypes (Row(..))
import SQLParser (Query(..))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

executeQuery :: Query -> [Row] -> [Row]
executeQuery (Query fields _ _) rows =
  let result = map (filterRow fields) rows
  in result

filterRow :: [Text] -> Row -> Row
filterRow fields (Row row) =
  Row $ HM.filterWithKey (\k _ -> k `elem` fields) row
