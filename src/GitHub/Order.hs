{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

@order@ input parameter (e.g. for sorting @issues@ or @milestones@).
-}

module GitHub.Order
    ( Order (..)
    , defIssueOrder
    , defMilestoneOrder

      -- * AST functions
    , maybeOrderToAst
    ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (maybeToList)
import Prolens (lens)

import GitHub.GraphQL (IssueOrderField (..), MilestoneOrderField (..), OrderDirection (..),
                       ParamName (..), ParamValue (..), QueryParam (..))
import GitHub.Lens (DirectionL (..), FieldL (..))
import GitHub.RequiredField (RequiredField (..))


{- | Connection parameter to specify order. Example:

https://docs.github.com/en/graphql/reference/input-objects#issueorder
-}
data Order (orderField :: Type) (fields :: [RequiredField]) = Order
   { orderDirection :: OrderDirection
   , orderField     :: orderField
   }

instance DirectionL (Order f) where
    directionL = lens orderDirection (\args new -> args { orderDirection = new })
    {-# INLINE directionL #-}

instance FieldL (Order f) f where
    fieldL = lens orderField (\args new -> args { orderField = new })
    {-# INLINE fieldL #-}

{- | Default value of 'Order'. Use methods of 'FieldL' and
'DirectionL' to change its fields.
-}
defIssueOrder :: Order IssueOrderField '[ 'FieldDirection, 'FieldField ]
defIssueOrder = Order
    { orderDirection = Asc
    , orderField = CreatedAt
    }

defMilestoneOrder :: Order MilestoneOrderField '[ 'FieldDirection, 'FieldField ]
defMilestoneOrder = Order
    { orderDirection = Asc
    , orderField = MNumber
    }

maybeOrderToAst :: (f -> ParamValue) -> Maybe (Order f '[]) -> [QueryParam]
maybeOrderToAst toParamValue = map (orderToAst toParamValue) . maybeToList

orderToAst :: (f -> ParamValue) -> Order f '[] -> QueryParam
orderToAst toParamValue Order{..} = QueryParam
    { queryParamName = ParamOrderBy
    , queryParamValue = ParamRecordV
        $ QueryParam
            { queryParamName = ParamField
            , queryParamValue = toParamValue orderField
            }
        :|
        [ QueryParam
            { queryParamName = ParamDirection
            , queryParamValue = ParamOrderDirection orderDirection
            }
        ]
    }
