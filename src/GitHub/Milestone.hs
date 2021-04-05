{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @milestones@.
-}

module GitHub.Milestone
    ( -- * Data types
      Milestones (..)
    , MilestonesArgs (..)
    , defMilestonesArgs

    , MilestoneField (..)

      -- * AST functions
    , milestonesToAst
    , milestoneToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Prolens (lens)

import GitHub.Connection (Connection (..), connectionToAst)
import GitHub.GraphQL (MilestoneOrderField, NodeName (..), ParamName (..), ParamValue (..),
                       QueryNode (..), QueryParam (..), mkQuery, nameNode)
import GitHub.Issue (Issues, issuesToAst)
import GitHub.Lens (LimitL (..), OrderL (..))
import GitHub.Order (Order, maybeOrderToAst)
import GitHub.RequiredField (RequiredField (..))


{- | The @milestones@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data Milestones = Milestones
    { milestonesArgs        :: MilestonesArgs '[]
    , milestonesConnections :: NonEmpty (Connection MilestoneField)
    }

milestonesToAst :: Milestones -> QueryNode
milestonesToAst Milestones{..} = QueryNode
    { queryNodeName = NodeMilestones
    , queryNodeArgs = milestonesArgsToAst milestonesArgs
    , queryNode     = mkQuery (connectionToAst milestoneFieldToAst) milestonesConnections
    }

{- | Arguments for the 'Milestones' connection.
-}
data MilestonesArgs (fields :: [RequiredField]) = MilestonesArgs
    { milestonesArgsLast    :: !Int
    , milestonesArgsOrderBy :: !(Maybe (Order MilestoneOrderField '[]))
    }

instance LimitL MilestonesArgs where
    lastL = lens milestonesArgsLast (\args new -> args { milestonesArgsLast = new })
    {-# INLINE lastL #-}

instance OrderL MilestonesArgs MilestoneOrderField where
    orderL = lens milestonesArgsOrderBy (\args new -> args { milestonesArgsOrderBy = new })
    {-# INLINE orderL #-}

{- | Default value of 'MilestonesArgs'. Use methods of 'HasLimit' to
change its fields.
-}
defMilestonesArgs :: MilestonesArgs '[ 'FieldLimit ]
defMilestonesArgs = MilestonesArgs
    { milestonesArgsLast = -1
    , milestonesArgsOrderBy = Nothing
    }

milestonesArgsToAst :: MilestonesArgs '[] -> [QueryParam]
milestonesArgsToAst MilestonesArgs{..} =
    [ QueryParam
        { queryParamName = ParamLast
        , queryParamValue = ParamIntV milestonesArgsLast
        }
    ]
    ++ maybeOrderToAst ParamMilestoneOrderField milestonesArgsOrderBy

{- | Fields of the @Milestone@ object.

* https://docs.github.com/en/graphql/reference/objects#milestone
-}
data MilestoneField
    = MilestoneId
    | MilestoneIssues Issues
    | MilestoneNumber
    | MilestoneProgressPercentage
    | MilestoneTitle

milestoneToAst :: NonEmpty MilestoneField -> QueryNode
milestoneToAst milestoneFields = QueryNode
    { queryNodeName = NodeMilestone
    , queryNodeArgs = []
    , queryNode     = mkQuery milestoneFieldToAst milestoneFields
    }

milestoneFieldToAst :: MilestoneField -> QueryNode
milestoneFieldToAst = \case
    MilestoneId                 -> nameNode NodeId
    MilestoneIssues issues      -> issuesToAst issues
    MilestoneNumber             -> nameNode NodeNumber
    MilestoneProgressPercentage -> nameNode NodeProgressPercentage
    MilestoneTitle              -> nameNode NodeTitle
