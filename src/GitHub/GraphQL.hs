{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GraphQL AST.
-}

module GitHub.GraphQL
    ( Query (..)
    , QueryNode (..)
    , NodeName (..)
    , QueryParam (..)
    , ParamName (..)
    , ParamValue (..)
    , State (..)
    , renderQuery
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)


newtype Query = Query
    { unQuery :: [QueryNode]
    }

data QueryNode = QueryNode
    { queryNodeName :: !NodeName
    , queryNodeArgs :: ![QueryParam]
    , queryNode     :: !Query
    }

data NodeName
    = NodeRepository
    | NodeIssues
    | NodeViewer
    | NodeTitle
    | NodeAuthor
    | NodeLogin

data QueryParam = QueryParam
    { queryParamName  :: !ParamName
    , queryParamValue :: !ParamValue
    }

data ParamName
    = ParamOwner
    | ParamName
    | ParamLast
    | ParamStates

data ParamValue
    = ParamStringV !Text
    | ParamIntV !Int
    | ParamStatesV !(NonEmpty State)

data State
    = Open
    | Closed
    | Merged

renderQuery :: Query -> Text
renderQuery _ = ""
