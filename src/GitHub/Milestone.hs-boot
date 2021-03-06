module GitHub.Milestone
    ( MilestoneField
    , milestoneToAst
    ) where

import Data.List.NonEmpty (NonEmpty)

import GitHub.GraphQL (QueryNode)


data MilestoneField

milestoneToAst :: NonEmpty MilestoneField -> QueryNode