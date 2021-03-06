{-# LANGUAGE DataKinds #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

@order@ input parameter (e.g. for sorting @issues@ or @milestones@).
-}

module GitHub.Order
    ( Order (..)
    , defOrder

      -- * AST functions
    , maybeOrderToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (maybeToList)
import Prolens (lens)

import GitHub.GraphQL (IssueOrderField (..), OrderDirection (..), ParamName (..), ParamValue (..),
                       QueryParam (..))
import GitHub.Lens (DirectionL (..), FieldL (..))
import GitHub.RequiredField (RequiredField (..))


{- | Connection parameter to specify order. Example:

https://docs.github.com/en/graphql/reference/input-objects#issueorder
-}
data Order (fields :: [RequiredField]) = Order
   { orderDirection :: OrderDirection
   , orderField     :: IssueOrderField
   }

instance DirectionL Order where
    directionL = lens orderDirection (\args new -> args { orderDirection = new })
    {-# INLINE directionL #-}

instance FieldL Order where
    fieldL = lens orderField (\args new -> args { orderField = new })
    {-# INLINE fieldL #-}

{- | Default value of 'Order'. Use methods of 'FieldL' and
'DirectionL' to change its fields.
-}
defOrder :: Order '[ 'FieldDirection, 'FieldField ]
defOrder = Order
    { orderDirection = Asc
    , orderField = CreatedAt
    }

maybeOrderToAst :: Maybe (Order '[]) -> [QueryParam]
maybeOrderToAst = map orderToAst . maybeToList

orderToAst :: Order '[] -> QueryParam
orderToAst Order{..} = QueryParam
    { queryParamName = ParamOrderBy
    , queryParamValue = ParamRecordV
        $ QueryParam
            { queryParamName = ParamField
            , queryParamValue = ParamIssueOrderField orderField
            }
        :|
        [ QueryParam
            { queryParamName = ParamDirection
            , queryParamValue = ParamOrderDirection orderDirection
            }
        ]
    }
