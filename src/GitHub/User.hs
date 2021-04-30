{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GitHub User type.
-}

module GitHub.User
    ( -- * Query
      Assignees (..)
    , AssigneesArgs (..)
    , defAssigneesArgs

    , Viewer (..)
    , viewerToAst

    , UserField (..)

      -- ** Internal
    , assigneesToAst

      -- * Mutation
    , AddAssigneesToAssignable (..)
    , addAssigneesToAssignableToAst
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Prolens (lens)

import GitHub.Connection (Connection, connectionToAst)
import GitHub.GraphQL (Mutation (..), MutationFun (..), NodeName (..), ParamName (..),
                       ParamValue (..), Query (..), QueryNode (..), QueryParam (..), mkQuery,
                       nameNode)
import GitHub.Id (Id (..))
import GitHub.Lens (LimitL (..))
import GitHub.RequiredField (RequiredField (..))


{- | @assignees@ connection of the issue type:

* https://docs.github.com/en/graphql/reference/objects#issue
-}
data Assignees = Assignees
    { assigneesArgs       :: AssigneesArgs '[]
    , assigneesConnection :: Connection UserField
    }

assigneesToAst :: Assignees -> QueryNode
assigneesToAst Assignees{..} = QueryNode
    { queryNodeName = NodeAssignees
    , queryNodeArgs = assigneesArgsToAst assigneesArgs
    , queryNode     = Query [connectionToAst userFieldToAst assigneesConnection]
    }

data AssigneesArgs (fields :: [RequiredField]) = AssigneesArgs
    { assigneesArgsLast :: Int
    }

instance LimitL AssigneesArgs where
    lastL = lens assigneesArgsLast (\args new -> args { assigneesArgsLast = new })
    {-# INLINE lastL #-}

{- | Default value of 'AssigneesArgs'. Use methods of 'LimitL' and
'StatesL' to change its fields.
-}
defAssigneesArgs :: AssigneesArgs '[ 'FieldLimit ]
defAssigneesArgs = AssigneesArgs
    { assigneesArgsLast = -1
    }

assigneesArgsToAst :: AssigneesArgs '[] -> [QueryParam]
assigneesArgsToAst AssigneesArgs{..} =
    [ QueryParam
        { queryParamName = ParamLast
        , queryParamValue = ParamIntV assigneesArgsLast
        }
    ]

newtype Viewer = Viewer
    { viewerFields :: NonEmpty UserField
    }

viewerToAst :: Viewer -> Query
viewerToAst Viewer{..} = Query
    [ QueryNode
        { queryNodeName = NodeViewer
        , queryNodeArgs = []
        , queryNode = mkQuery userFieldToAst viewerFields
        }
    ]

data UserField
    = UserId
    | UserLogin

userFieldToAst :: UserField -> QueryNode
userFieldToAst = \case
    UserId    -> nameNode NodeId
    UserLogin -> nameNode NodeLogin

{- Mutation to add assigneess.

* https://docs.github.com/en/graphql/reference/mutations#addassigneestoassignable
-}
data AddAssigneesToAssignable where
    AddAssigneesToAssignable
        :: forall to who
        .  { assignTo :: Id to
           , assignees :: [Id who]
           }
        -> AddAssigneesToAssignable

addAssigneesToAssignableToAst :: AddAssigneesToAssignable -> Mutation
addAssigneesToAssignableToAst AddAssigneesToAssignable{..} = Mutation
    [ MutationFun
        { mutationFunName = NodeAddAssigneesToAssignable
        , mutationFunInput =
            QueryParam
                { queryParamName = ParamAssignableId
                , queryParamValue = ParamStringV (unId assignTo)
                }
            :|
            [ QueryParam
                { queryParamName = ParamAssigneeIds
                , queryParamValue = ParamArrayV (map (ParamStringV . unId) assignees)
                }
            ]
        , mutationFunReturning = [ nameNode NodeClientMutationId ]
        }
    ]
