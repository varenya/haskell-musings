module PrettyPrint where

import qualified Data.Text   as Text
import           Data.Tree
import           Text.Printf

import           Project
import           Reporting

asTree :: Project -> Tree String
asTree (Project (ProjectId id) name) = Node (printf "%s (%d)" name id) []
asTree (ProjectGroup name projects) =
  Node (Text.unpack name) (map asTree projects)

prettyProject :: Project -> String
prettyProject = drawTree . asTree

prettyReport :: Report -> String
prettyReport report =
  printf
    "Budget: %.2f , Net : %.2f , Difference %+.2f"
    (unMoney (budgetProfit report))
    (unMoney (netProfit report))
    (unMoney (difference report))
