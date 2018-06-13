module Database where

import           Project
import           System.Random (getStdRandom, randomR)

getBudget :: ProjectId -> IO Budget
getBudget _ = do
  income <- Money <$> getStdRandom (randomR (1, 10000))
  expenditure <- Money <$> getStdRandom (randomR (1, 10000))
  pure Budget {budgetIncome = income, budgetExpenditure = expenditure}

getTransactions :: ProjectId -> IO [Transaction]
getTransactions _ = do
    sale <- Sale . Money <$> getStdRandom (randomR (1,4000))
    purchase <- Sale . Money <$> getStdRandom (randomR (1,4000))
    return [sale,purchase]
