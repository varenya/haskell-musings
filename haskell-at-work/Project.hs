{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Project where

import           Data.Text (Text)

newtype Money = Money
  { unMoney :: Double
  } deriving (Eq, Show, Num)

newtype ProjectId = ProjectId
  { unProjectId :: Int
  } deriving (Eq, Show, Num)

data Project a
  = Project Text
            a
  | ProjectGroup Text
                 [Project a]
  deriving (Eq, Show, Traversable, Foldable, Functor)

data Budget = Budget
  { budgetIncome      :: Money
  , budgetExpenditure :: Money
  } deriving (Eq, Show)

data Transaction
  = Sale Money
  | Purchase Money
  deriving (Eq, Show)
