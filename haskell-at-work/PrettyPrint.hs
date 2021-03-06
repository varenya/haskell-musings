module PrettyPrint where

import qualified Data.Text   as Text
import           Data.Tree
import           Text.Printf

import           Project
import           Reporting

asTree :: (a -> String) -> Project a -> Tree String
asTree prettyValue (Project name x) =
  Node (printf "%s %s" name (prettyValue x)) []
asTree prettyValue (ProjectGroup name projects) =
  Node (Text.unpack name) (map (asTree prettyValue) projects)

prettyProject :: (a -> String) -> Project a -> String
prettyProject prettyValue = drawTree . (asTree prettyValue)

prettyReport :: Report -> String
prettyReport report =
  printf
    "Budget: %.2f , Net : %.2f , Difference %+.2f"
    (unMoney (budgetProfit report))
    (unMoney (netProfit report))
    (unMoney (difference report))
