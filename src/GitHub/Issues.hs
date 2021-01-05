{-# LANGUAGE DataKinds #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @issues@.
-}

module GitHub.Issues
    ( -- * Data types
      Issues (..)
    , IssuesArgs (..)
    , defIssuesArgs

    , IssueField (..)

      -- * AST functions
    , issuesToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Prolens (lens)

import {-# SOURCE #-} GitHub.Author (AuthorField, authorToAst)
import GitHub.Connection (Connection (..), connectionToAst)
import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), QueryNode (..),
                       QueryParam (..), State (..), mkQuery, nameNode)
import GitHub.Lens (ArgsType (..), HasLimit (..), HasStates (..))


{- | The @issues@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data Issues = Issues
    { issuesArgs        :: !(IssuesArgs '[])
    , issuesConnections :: !(NonEmpty (Connection IssueField))
    }

issuesToAst :: Issues -> QueryNode
issuesToAst Issues{..} = QueryNode
    { queryNodeName = NodeIssues
    , queryNodeArgs = issuesArgsToAst issuesArgs
    , queryNode     = mkQuery (connectionToAst issueFieldToAst) issuesConnections
    }

{- | Arguments for the 'Issues' connection.
-}
data IssuesArgs (args :: [ArgsType]) = IssuesArgs
    { issuesArgsLast   :: !Int
    , issuesArgsStates :: !(NonEmpty State)
    }

instance HasLimit IssuesArgs where
    lastL = lens issuesArgsLast (\args new -> args { issuesArgsLast = new })
    {-# INLINE lastL #-}

instance HasStates IssuesArgs where
    statesL = lens issuesArgsStates (\args new -> args { issuesArgsStates = new })
    {-# INLINE statesL #-}

{- | Default value of 'IssuesArgs'. Use methods of 'HasLimit' and
'HasStates' to change its fields.
-}
defIssuesArgs :: IssuesArgs '[ 'ArgsLimit, 'ArgsStates ]
defIssuesArgs = IssuesArgs
    { issuesArgsLast = -1
    , issuesArgsStates = Open :| []
    }

issuesArgsToAst :: IssuesArgs '[] -> [QueryParam]
issuesArgsToAst IssuesArgs{..} =
    [ QueryParam
        { queryParamName = ParamLast
        , queryParamValue = ParamIntV issuesArgsLast
        }
    , QueryParam
        { queryParamName = ParamStates
        , queryParamValue = ParamStatesV issuesArgsStates
        }
    ]

{- | Fields of the @Issue@ object.

* https://developer.github.com/v4/object/issue/
-}
data IssueField
    = IssueTitle
    | IssueAuthor (NonEmpty AuthorField)

issueFieldToAst :: IssueField -> QueryNode
issueFieldToAst = \case
    IssueTitle               -> nameNode NodeTitle
    IssueAuthor authorFields -> authorToAst authorFields
