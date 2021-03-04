{-# LANGUAGE DataKinds #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

GitHub User type.
-}

module GitHub.User
    ( Assignees (..)
    , AssigneesArgs (..)
    , defAssigneesArgs

    , UserField (..)

      -- * Internal
    , assigneesToAst
    ) where

import Prolens (lens)

import GitHub.Connection (Connection, connectionToAst)
import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), Query (..), QueryNode (..),
                       QueryParam (..), nameNode)
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

data UserField
    = UserLogin

userFieldToAst :: UserField -> QueryNode
userFieldToAst = \case
    UserLogin -> nameNode NodeLogin
