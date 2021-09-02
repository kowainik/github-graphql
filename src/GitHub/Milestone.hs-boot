module GitHub.Milestone
    ( MilestoneField
    , milestoneFieldsToAst
    ) where

import Data.List.NonEmpty (NonEmpty)

import GitHub.GraphQL (QueryNode)


data MilestoneField

milestoneFieldsToAst :: NonEmpty MilestoneField -> QueryNode