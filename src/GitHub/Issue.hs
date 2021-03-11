{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @issues@.
-}

module GitHub.Issue
    ( -- * Query
      -- ** Data types
      Issue (..)
    , IssueArgs (..)
    , defIssueArgs

    , Issues (..)
    , IssuesArgs (..)
    , defIssuesArgs

    , IssueField (..)

      -- ** AST functions
    , issueToAst
    , issuesToAst

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
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Prolens (Lens', lens)

import {-# SOURCE #-} GitHub.Author (AuthorField, authorToAst)
import GitHub.Connection (Connection (..), connectionToAst)
import GitHub.GraphQL (IssueState (..), Mutation (..), MutationFun (..), NodeName (..),
                       ParamName (..), ParamValue (..), QueryNode (..), QueryParam (..), mkQuery,
                       nameNode)
import GitHub.Id (Id (..), MilestoneId, RepositoryId)
import GitHub.Label (Labels, labelsToAst)
import GitHub.Lens (LimitL (..), NumberL (..), OrderL (..), RepositoryIdL (..), StatesL (..),
                    TitleL (..))
import {-# SOURCE #-} GitHub.Milestone (MilestoneField, milestoneToAst)
import GitHub.Order (Order, maybeOrderToAst)
import GitHub.RequiredField (RequiredField (..))
import GitHub.User (Assignees, assigneesToAst)

{- | The @issue@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data Issue = Issue
    { issueArgs        :: IssueArgs '[]
    , issueConnections :: NonEmpty (Connection IssueField)
    }

issueToAst :: Issue -> QueryNode
issueToAst Issue{..} = QueryNode
    { queryNodeName = NodeIssue
    , queryNodeArgs = issueArgsToAst issueArgs
    , queryNode     = mkQuery (connectionToAst issueFieldToAst) issueConnections
    }

{- | Arguments for the 'Issues' connection.
-}
data IssueArgs (fields :: [RequiredField]) = IssueArgs
    { issueArgsNumber :: Int
    }

instance NumberL IssueArgs where
    numberL = lens issueArgsNumber (\args new -> args { issueArgsNumber = new })
    {-# INLINE numberL #-}

{- | Default value of 'IssueArgs'. Use methods of 'NumberL' to change
its fields.
-}
defIssueArgs :: IssueArgs '[ 'FieldNumber ]
defIssueArgs = IssueArgs
    { issueArgsNumber = -1
    }

issueArgsToAst :: IssueArgs '[] -> [QueryParam]
issueArgsToAst IssueArgs{..} =
    [ QueryParam
        { queryParamName = ParamNumber
        , queryParamValue = ParamIntV issueArgsNumber
        }
    ]
{- | The @issues@ connection of the 'Repository' object.

* https://developer.github.com/v4/object/repository/#connections
-}
data Issues = Issues
    { issuesArgs        :: IssuesArgs '[]
    , issuesConnections :: NonEmpty (Connection IssueField)
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
    { issuesArgsLast    :: Int
    , issuesArgsStates  :: NonEmpty IssueState
    , issuesArgsOrderBy :: Maybe (Order '[])
    }

instance LimitL IssuesArgs where
    lastL = lens issuesArgsLast (\args new -> args { issuesArgsLast = new })
    {-# INLINE lastL #-}

instance StatesL IssuesArgs IssueState where
    statesL = lens issuesArgsStates (\args new -> args { issuesArgsStates = new })
    {-# INLINE statesL #-}

instance OrderL IssuesArgs where
    orderL = lens issuesArgsOrderBy (\args new -> args { issuesArgsOrderBy = new })
    {-# INLINE orderL #-}

{- | Default value of 'IssuesArgs'. Use methods of 'LimitL' and
'StatesL' to change its fields.
-}
defIssuesArgs :: IssuesArgs '[ 'FieldLimit, 'FieldStates ]
defIssuesArgs = IssuesArgs
    { issuesArgsLast    = -1
    , issuesArgsStates  = IssueOpen :| []
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
        , queryParamValue = ParamIssueStatesV issuesArgsStates
        }
    ]
    ++ maybeOrderToAst issuesArgsOrderBy


{- | Fields of the @Issue@ object.

* https://developer.github.com/v4/object/issue/
-}
data IssueField
    = IssueAssignees Assignees
    | IssueAuthor (NonEmpty AuthorField)
    | IssueBody
    | IssueId
    | IssueLabels Labels
    | IssueMilestone (NonEmpty MilestoneField)
    | IssueNumber
    | IssueState
    | IssueTitle
    | IssueUrl

issueFieldToAst :: IssueField -> QueryNode
issueFieldToAst = \case
    IssueAuthor authorFields       -> authorToAst authorFields
    IssueAssignees assignees       -> assigneesToAst assignees
    IssueBody                      -> nameNode NodeBody
    IssueId                        -> nameNode NodeId
    IssueLabels labels             -> labelsToAst labels
    IssueMilestone milestoneFields -> milestoneToAst milestoneFields
    IssueNumber                    -> nameNode NodeNumber
    IssueState                     -> nameNode NodeState
    IssueTitle                     -> nameNode NodeTitle
    IssueUrl                       -> nameNode NodeUrl

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
            [ QueryNode
                { queryNodeName = NodeIssue
                , queryNodeArgs = []
                , queryNode     = mkQuery issueFieldToAst createIssueFields
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
    ++ map
        (\(Id milestoneId) -> QueryParam
            { queryParamName = ParamMilestoneId
            , queryParamValue = ParamStringV milestoneId
            }
        )
        (maybeToList createIssueInputMilestoneId)
