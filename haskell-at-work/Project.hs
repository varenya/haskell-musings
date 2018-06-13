{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Project where

import           Data.Text (Text)

newtype Money = Money
  { unMoney :: Double
  } deriving (Eq, Show, Num)

newtype ProjectId = ProjectId
  { unProjectId :: Int
  } deriving (Eq, Show, Num)

data Project
  = Project ProjectId
            Text
  | ProjectGroup Text
                 [Project]
  deriving (Eq, Show)

data Budget = Budget
  { budgetIncome      :: Money
  , budgetExpenditure :: Money
  } deriving (Eq, Show)

data Transaction
  = Sale Money
  | Purchase Money
  deriving (Eq, Show)
