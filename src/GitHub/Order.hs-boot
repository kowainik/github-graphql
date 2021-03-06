{-# LANGUAGE DataKinds #-}

module GitHub.Order
    ( Order
    ) where

import GitHub.RequiredField (RequiredField (..))
import GitHub.GraphQL (OrderDirection, IssueOrderField)

data Order (fields :: [RequiredField]) = Order
   { orderDirection :: OrderDirection
   , orderField     :: IssueOrderField
   }
