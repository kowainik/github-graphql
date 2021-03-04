{-# LANGUAGE DataKinds #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Issue/PR label type.
-}

module GitHub.Label
    ( Labels (..)
    , LabelsArgs (..)
    , defLabelsArgs

    , LabelField (..)

      -- * Internal
    , labelsToAst
    ) where

import Prolens (lens)

import GitHub.Connection (Connection, connectionToAst)
import GitHub.GraphQL (NodeName (..), ParamName (..), ParamValue (..), Query (..), QueryNode (..),
                       QueryParam (..), nameNode)
import GitHub.Lens (LimitL (..))
import GitHub.RequiredField (RequiredField (..))


{- | @labels@ connection of the issue type:

* https://docs.github.com/en/graphql/reference/objects#issue
-}
data Labels = Labels
    { labelsArgs       :: LabelsArgs '[]
    , labelsConnection :: Connection LabelField
    }

labelsToAst :: Labels -> QueryNode
labelsToAst Labels{..} = QueryNode
    { queryNodeName = NodeLabels
    , queryNodeArgs = labelsArgsToAst labelsArgs
    , queryNode     = Query [connectionToAst labelFieldToAst labelsConnection]
    }

data LabelsArgs (fields :: [RequiredField]) = LabelsArgs
    { labelsArgsLast :: Int
    }

instance LimitL LabelsArgs where
    lastL = lens labelsArgsLast (\args new -> args { labelsArgsLast = new })
    {-# INLINE lastL #-}

{- | Default value of 'LabelsArgs'. Use methods of 'LimitL' and
'StatesL' to change its fields.
-}
defLabelsArgs :: LabelsArgs '[ 'FieldLimit ]
defLabelsArgs = LabelsArgs
    { labelsArgsLast = -1
    }

labelsArgsToAst :: LabelsArgs '[] -> [QueryParam]
labelsArgsToAst LabelsArgs{..} =
    [ QueryParam
        { queryParamName = ParamLast
        , queryParamValue = ParamIntV labelsArgsLast
        }
    ]

data LabelField
    = LabelName

labelFieldToAst :: LabelField -> QueryNode
labelFieldToAst = \case
    LabelName -> nameNode NodeName
