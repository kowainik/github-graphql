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
import GHC.TypeLits (ErrorMessage (..), Symbol)
import Prolens (lens)
import Type.Errors.Pretty (TypeError, type (%), type (<>))

import GitHub.Connection (Connection (..))
import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), Query (..), QueryNode (..),
                       QueryParam (..), mkQuery)
import GitHub.Issues (IssueField, Issues (..), IssuesArgs, issuesToAst)
import GitHub.Lens (ArgsType (..), HasOwnerName (..))
import GitHub.PullRequests (PullRequestField, PullRequests (..), PullRequestsArgs,
                            pullRequestsToAst)


{- | The @Repository@ top-level connection:

* https://developer.github.com/v4/query/#connections
-}
data Repository where
    Repository
        :: forall args
        .  HasNoRepositoryArgs args
        => { repositoryArgs   :: !(RepositoryArgs args)
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
data RepositoryArgs (args :: [ArgsType]) = RepositoryArgs
    { repositoryArgsOwner :: !Text
    , repositoryArgsName  :: !Text
    }

instance HasOwnerName RepositoryArgs where
    ownerL = lens repositoryArgsOwner (\args new -> args { repositoryArgsOwner = new })
    {-# INLINE ownerL #-}
    nameL = lens repositoryArgsName (\args new -> args { repositoryArgsName = new })
    {-# INLINE nameL #-}

{- | Default value of 'RepositoryArgs'. Use methods of 'HasOwnerName'
to change its fields.
-}
defRepositoryArgs :: RepositoryArgs [ 'ArgsOwner, 'ArgsName ]
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

repositoryFieldToAst :: RepositoryField -> QueryNode
repositoryFieldToAst = \case
    RepositoryIssues issuesField             -> issuesToAst issuesField
    RepositoryPullRequests pullRequestsField -> pullRequestsToAst pullRequestsField

{- | Smart constructor for the 'Repository' type.
-}
repository
    :: HasNoRepositoryArgs args
    => RepositoryArgs args
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

type family HasNoRepositoryArgs (args :: [ArgsType]) :: Constraint where
    HasNoRepositoryArgs '[]  = (() :: Constraint)
    HasNoRepositoryArgs args = TypeError
        ( "You haven't set the following required fields of 'RepositoryArgs':"
        % ""
        % PrintArgs args
        % "Use corresponding lenses to set values of the fields"
        )

type family PrintArgs (args :: [ArgsType]) :: ErrorMessage where
    PrintArgs '[] = 'Text ""
    PrintArgs (x ': xs) = ("    * " <> ShowArg x) % PrintArgs xs

type family ShowArg (arg :: ArgsType) :: Symbol where
    ShowArg 'ArgsOwner  = "owner"
    ShowArg 'ArgsName   = "name"
    ShowArg 'ArgsLimit  = "last or first"
    ShowArg 'ArgsStates = "states"
