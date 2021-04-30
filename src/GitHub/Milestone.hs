{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @milestones@.
-}

module GitHub.Milestone
    ( -- * Query
      -- ** Data types
      Milestone (..)
    , MilestoneArgs (..)
    , defMilestoneArgs

    , Milestones (..)
    , MilestonesArgs (..)
    , defMilestonesArgs

    , MilestoneField (..)

      -- * AST functions
    , milestoneToAst
    , milestonesToAst
    , milestoneFieldsToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Prolens (lens)

import GitHub.Connection (Connection (..), connectionToAst)
import GitHub.GraphQL (IssueState (..), MilestoneOrderField, NodeName (..), ParamName (..),
                       ParamValue (..), QueryNode (..), QueryParam (..), mkQuery, nameNode)
import GitHub.Issue (Issues, issuesToAst)
import GitHub.Lens (LimitL (..), NumberL (..), OrderL (..), StatesL (..))
import GitHub.Order (Order, maybeOrderToAst)
import GitHub.RequiredField (RequiredField (..))

----------------------------------------------------------------------------
-- Single milestone
----------------------------------------------------------------------------

{- | The @milestone@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data Milestone = Milestone
    { milestoneArgs        :: MilestoneArgs '[]
    , milestoneConnections :: NonEmpty MilestoneField
    }

milestoneToAst :: Milestone -> QueryNode
milestoneToAst Milestone{..} = QueryNode
    { queryNodeName = NodeMilestone
    , queryNodeArgs = milestoneArgsToAst milestoneArgs
    , queryNode     = mkQuery milestoneFieldToAst milestoneConnections
    }

{- | Arguments for the 'Milestone' connection.
-}
newtype MilestoneArgs (fields :: [RequiredField]) = MilestoneArgs
    { milestoneArgsNumber    :: Int
    }

instance NumberL MilestoneArgs where
    numberL = lens milestoneArgsNumber (\args new -> args { milestoneArgsNumber = new })
    {-# INLINE numberL #-}

{- | Default value of 'MilestoneArgs'. Use methods of 'HasNumber' to
change its fields.
-}
defMilestoneArgs :: MilestoneArgs '[ 'FieldNumber ]
defMilestoneArgs = MilestoneArgs
    { milestoneArgsNumber = -1
    }

milestoneArgsToAst :: MilestoneArgs '[] -> [QueryParam]
milestoneArgsToAst MilestoneArgs{..} =
    [ QueryParam
        { queryParamName = ParamNumber
        , queryParamValue = ParamIntV milestoneArgsNumber
        }
    ]

----------------------------------------------------------------------------
-- Multiple milestones
----------------------------------------------------------------------------

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
    { milestonesArgsLast    :: Int
    , milestonesArgsStates  :: NonEmpty IssueState
    , milestonesArgsOrderBy :: Maybe (Order MilestoneOrderField '[])
    }

instance LimitL MilestonesArgs where
    lastL = lens milestonesArgsLast (\args new -> args { milestonesArgsLast = new })
    {-# INLINE lastL #-}

instance StatesL MilestonesArgs IssueState where
    statesL = lens milestonesArgsStates (\args new -> args { milestonesArgsStates = new })
    {-# INLINE statesL #-}

instance OrderL MilestonesArgs MilestoneOrderField where
    orderL = lens milestonesArgsOrderBy (\args new -> args { milestonesArgsOrderBy = new })
    {-# INLINE orderL #-}

{- | Default value of 'MilestonesArgs'. Use methods of 'HasLimit' to
change its fields.
-}
defMilestonesArgs :: MilestonesArgs '[ 'FieldLimit ]
defMilestonesArgs = MilestonesArgs
    { milestonesArgsLast = -1
    , milestonesArgsStates = IssueOpen :| [ IssueClosed ]
    , milestonesArgsOrderBy = Nothing
    }

milestonesArgsToAst :: MilestonesArgs '[] -> [QueryParam]
milestonesArgsToAst MilestonesArgs{..} =
    [ QueryParam
        { queryParamName = ParamLast
        , queryParamValue = ParamIntV milestonesArgsLast
        }
    , QueryParam
        { queryParamName = ParamStates
        , queryParamValue = ParamIssueStatesV milestonesArgsStates
        }
    ]
    ++ maybeOrderToAst ParamMilestoneOrderField milestonesArgsOrderBy

----------------------------------------------------------------------------
-- Milestone fields
----------------------------------------------------------------------------

{- | Fields of the @Milestone@ object.

* https://docs.github.com/en/graphql/reference/objects#milestone
-}
data MilestoneField
    = MilestoneId
    | MilestoneIssues Issues
    | MilestoneNumber
    | MilestoneProgressPercentage
    | MilestoneTitle
    | MilestoneDescription

milestoneFieldsToAst :: NonEmpty MilestoneField -> QueryNode
milestoneFieldsToAst milestoneFields = QueryNode
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
    MilestoneDescription        -> nameNode NodeDescription
