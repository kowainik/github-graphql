{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @repository@.
-}

module GitHub.Repository
    ( -- * Data types
      Repository (..)
    , RepositoryArgs (..)
    , RepositoryField (..)
    , defRepositoryArgs

      -- * Smart constructors
    , repository
    , issues
    , pullRequests

      -- * AST functions
    , repositoryToAst
    ) where

import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Prolens (lens)
import Type.Errors.Pretty (TypeError, type (%))

import GitHub.Connection (Connection (..))
import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), Query (..), QueryNode (..),
                       QueryParam (..), mkQuery, nameNode)
import GitHub.Issues (IssueField, Issues (..), IssuesArgs, issuesToAst)
import GitHub.Lens (NameL (..), OwnerL (..))
import GitHub.PullRequests (PullRequestField, PullRequests (..), PullRequestsArgs,
                            pullRequestsToAst)
import GitHub.RequiredField (DisplayFields, RequiredField (..))


{- | The @Repository@ top-level connection:

* https://developer.github.com/v4/query/#connections
-}
data Repository where
    Repository
        :: forall fields
        .  CheckFieldsRepositoryArgs fields
        => { repositoryArgs   :: !(RepositoryArgs fields)
           , repositoryFields :: !(NonEmpty RepositoryField)
           }
        -> Repository

repositoryToAst :: Repository -> Query
repositoryToAst Repository{..} = Query
    [ QueryNode
        { queryNodeName = NodeRepository
        , queryNodeArgs = repositoryArgsToAst repositoryArgs
        , queryNode     = mkQuery repositoryFieldToAst repositoryFields
        }
    ]

{- | Arguments for the 'Repository' connection.
-}
data RepositoryArgs (fields :: [RequiredField]) = RepositoryArgs
    { repositoryArgsOwner :: !Text
    , repositoryArgsName  :: !Text
    }

instance OwnerL RepositoryArgs where
    ownerL = lens repositoryArgsOwner (\args new -> args { repositoryArgsOwner = new })
    {-# INLINE ownerL #-}

instance NameL RepositoryArgs where
    nameL = lens repositoryArgsName (\args new -> args { repositoryArgsName = new })
    {-# INLINE nameL #-}

{- | Default value of 'RepositoryArgs'. Use methods of 'HasOwnerName'
to change its fields.
-}
defRepositoryArgs :: RepositoryArgs [ 'FieldOwner, 'FieldName ]
defRepositoryArgs = RepositoryArgs
    { repositoryArgsOwner = ""
    , repositoryArgsName  = ""
    }

repositoryArgsToAst :: RepositoryArgs args -> [QueryParam]
repositoryArgsToAst RepositoryArgs{..} =
    [ QueryParam
        { queryParamName = ParamOwner
        , queryParamValue = ParamStringV repositoryArgsOwner
        }
    , QueryParam
        { queryParamName = ParamName
        , queryParamValue = ParamStringV repositoryArgsName
        }
    ]

{- | Fields and connections of the @Repository@ object:

* https://developer.github.com/v4/object/repository/
-}
data RepositoryField
    = RepositoryIssues Issues
    | RepositoryPullRequests PullRequests
    | RepositoryId

repositoryFieldToAst :: RepositoryField -> QueryNode
repositoryFieldToAst = \case
    RepositoryIssues issuesField             -> issuesToAst issuesField
    RepositoryPullRequests pullRequestsField -> pullRequestsToAst pullRequestsField
    RepositoryId                             -> nameNode NodeId

{- | Smart constructor for the 'Repository' type.
-}
repository
    :: CheckFieldsRepositoryArgs fields
    => RepositoryArgs fields
    -> NonEmpty RepositoryField
    -> Repository
repository repositoryArgs repositoryFields = Repository{..}

{- | Smart constructor for the 'Issue' field of the 'RepositoryField'.
-}
issues :: IssuesArgs '[] -> NonEmpty (Connection IssueField) -> RepositoryField
issues issuesArgs issuesConnections = RepositoryIssues Issues{..}

{- | Smart constructor for the 'PullRequest' field of the 'RepositoryField'.
-}
pullRequests
    :: PullRequestsArgs '[]
    -> NonEmpty (Connection PullRequestField)
    -> RepositoryField
pullRequests pullRequestsArgs pullRequestsConnections =
    RepositoryPullRequests PullRequests{..}

type family CheckFieldsRepositoryArgs (fields :: [RequiredField]) :: Constraint where
    CheckFieldsRepositoryArgs '[]  = (() :: Constraint)
    CheckFieldsRepositoryArgs fields = TypeError
        ( "You haven't set the following required fields of 'RepositoryArgs':"
        % ""
        % DisplayFields fields
        % "Use corresponding lenses to set values of the fields."
        % "Typically, you set values of this type using lenses like so:"
        % ""
        % "    defRepositoryArgs"
        % "    & set ownerL \"owner-name\""
        % "    & set nameL \"repository-name\""
        % ""
        )
