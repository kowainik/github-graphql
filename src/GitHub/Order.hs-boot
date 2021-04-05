{-# LANGUAGE DataKinds #-}

module GitHub.Order
    ( Order
    ) where

import Data.Kind (Type)

import GitHub.RequiredField (RequiredField (..))
import GitHub.GraphQL (OrderDirection)

data Order (orderField :: Type) (fields :: [RequiredField]) = Order
   { orderDirection :: OrderDirection
   , orderField     :: orderField
   }
