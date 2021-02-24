{-# LANGUAGE DataKinds #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @issues@.
-}

module GitHub.Issues
    ( -- * Query
      -- ** Data types
      Issues (..)
    , IssuesArgs (..)
    , defIssuesArgs
    , issueOrderL

    , IssueOrder (..)
    , defIssueOrder

    , IssueField (..)

      -- ** AST functions
    , issuesToAst
    , issueOrderToAst

      -- * Mutation
      -- ** Data types
    , CreateIssue (..)
    , CreateIssueInput (..)
    , defCreateIssueInput
    , milestoneIdL

      -- ** AST functions
    , createIssueToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Prolens (Lens', lens)

import {-# SOURCE #-} GitHub.Author (AuthorField, authorToAst, authorToMutationNode)
import GitHub.Connection (Connection (..), connectionToAst)
import GitHub.GraphQL (IssueOrderField (..), Mutation (..), MutationFun (..), MutationNode (..),
                       NodeName (..), OrderDirection (..), ParamName (..), ParamValue (..),
                       QueryNode (..), QueryParam (..), State (..), mkQuery, nameMutationNode,
                       nameNode)
import GitHub.Id (Id (..), MilestoneId, RepositoryId)
import GitHub.Lens (DirectionL (..), FieldL (..), LimitL (..), RepositoryIdL (..), StatesL (..),
                    TitleL (..))
import GitHub.RequiredField (RequiredField (..))


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
data IssuesArgs (fields :: [RequiredField]) = IssuesArgs
    { issuesArgsLast    :: !Int
    , issuesArgsStates  :: !(NonEmpty State)
    , issuesArgsOrderBy :: !(Maybe (IssueOrder '[]))
    }

instance LimitL IssuesArgs where
    lastL = lens issuesArgsLast (\args new -> args { issuesArgsLast = new })
    {-# INLINE lastL #-}

instance StatesL IssuesArgs where
    statesL = lens issuesArgsStates (\args new -> args { issuesArgsStates = new })
    {-# INLINE statesL #-}

issueOrderL :: Lens' (IssuesArgs fields) (Maybe (IssueOrder '[]))
issueOrderL = lens issuesArgsOrderBy (\args new -> args { issuesArgsOrderBy = new })

{- | Default value of 'IssuesArgs'. Use methods of 'LimitL' and
'StatesL' to change its fields.
-}
defIssuesArgs :: IssuesArgs '[ 'FieldLimit, 'FieldStates ]
defIssuesArgs = IssuesArgs
    { issuesArgsLast    = -1
    , issuesArgsStates  = Open :| []
    , issuesArgsOrderBy = Nothing
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
    ++ maybe [] (\io -> [issueOrderToAst io]) issuesArgsOrderBy

{- | Connection parameter to specify issue order:

https://docs.github.com/en/graphql/reference/input-objects#issueorder
-}
data IssueOrder (fields :: [RequiredField]) = IssueOrder
   { issueOrderDirection :: !OrderDirection
   , issueOrderField     :: !IssueOrderField
   }

instance DirectionL IssueOrder where
    directionL = lens issueOrderDirection (\args new -> args { issueOrderDirection = new })
    {-# INLINE directionL #-}

instance FieldL IssueOrder where
    fieldL = lens issueOrderField (\args new -> args { issueOrderField = new })
    {-# INLINE fieldL #-}

{- | Default value of 'IssuesOrder'. Use methods of 'FieldL' and
'DirectionL' to change its fields.
-}
defIssueOrder :: IssueOrder '[ 'FieldDirection, 'FieldField ]
defIssueOrder = IssueOrder
    { issueOrderDirection = Asc
    , issueOrderField = CreatedAt
    }

issueOrderToAst :: IssueOrder '[] -> QueryParam
issueOrderToAst IssueOrder{..} = QueryParam
    { queryParamName = ParamOrderBy
    , queryParamValue = ParamRecordV
        $ QueryParam
            { queryParamName = ParamField
            , queryParamValue = ParamIssueOrderField issueOrderField
            }
        :|
        [ QueryParam
            { queryParamName = ParamDirection
            , queryParamValue = ParamOrderDirection issueOrderDirection
            }
        ]
    }

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

issueFieldToMutationNode :: IssueField -> MutationNode
issueFieldToMutationNode = \case
    IssueTitle               -> nameMutationNode NodeTitle
    IssueAuthor authorFields -> authorToMutationNode authorFields

{- | Data type for creating issue.

* https://docs.github.com/en/graphql/reference/mutations#createissue
-}
data CreateIssue = CreateIssue
    { createIssueInput  :: !(CreateIssueInput '[])
    , createIssueFields :: ![IssueField]
    }

createIssueToAst :: CreateIssue -> Mutation
createIssueToAst CreateIssue{..} = Mutation
    [ MutationFun
        { mutationFunName = NodeCreateIssue
        , mutationFunInput = createIssueInputToAst createIssueInput
        , mutationFunReturning =
            [ MutationNode
                { mutationNodeName = NodeIssue
                , mutationNodeChildren = map issueFieldToMutationNode createIssueFields
                }
            ]
        }
    ]

{- | Arguments for the 'CreateIssue' mutation.

* https://docs.github.com/en/graphql/reference/input-objects#createissueinput
-}
data CreateIssueInput (fields :: [RequiredField]) = CreateIssueInput
    { createIssueInputRepositoryId :: !RepositoryId
    , createIssueInputTitle        :: !Text
    , createIssueInputMilestoneId  :: !(Maybe MilestoneId)
    }

instance RepositoryIdL CreateIssueInput where
    repositoryIdL = lens createIssueInputRepositoryId (\args new -> args { createIssueInputRepositoryId = new })
    {-# INLINE repositoryIdL #-}

instance TitleL CreateIssueInput where
    titleL = lens createIssueInputTitle (\args new -> args { createIssueInputTitle = new })
    {-# INLINE titleL #-}

milestoneIdL :: Lens' (CreateIssueInput fields) (Maybe MilestoneId)
milestoneIdL = lens
    createIssueInputMilestoneId
    (\args new -> args { createIssueInputMilestoneId = new })

{- | Default value of 'CreateIssueInput'. Use methods of 'TitleL' and
'RepositoryIdL' to change its fields.
-}
defCreateIssueInput :: CreateIssueInput '[ 'FieldRepositoryId, 'FieldTitle ]
defCreateIssueInput = CreateIssueInput
    { createIssueInputRepositoryId = Id ""
    , createIssueInputTitle = ""
    , createIssueInputMilestoneId = Nothing
    }

createIssueInputToAst :: CreateIssueInput '[] -> NonEmpty QueryParam
createIssueInputToAst CreateIssueInput{..} =
    QueryParam
        { queryParamName = ParamRepositoryId
        , queryParamValue = ParamStringV $ unId createIssueInputRepositoryId
        }
    :|
    [ QueryParam
        { queryParamName = ParamTitle
        , queryParamValue = ParamStringV createIssueInputTitle
        }
    ]
    ++ maybe
        []
        (\(Id milestoneId) ->
            [ QueryParam
                { queryParamName = ParamMilestoneId
                , queryParamValue = ParamStringV milestoneId
                }
            ]
        )
        createIssueInputMilestoneId
