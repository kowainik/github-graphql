{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds           #-}

{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Diverse general-purpose utilities.
-}

module GitHub.Common
    ( one
    , typeName
    ) where

import Data.List.NonEmpty (NonEmpty (..))
import Type.Reflection (Typeable, typeRep)


{- | Helper function to construct singleton 'NonEmpty' lists.
-}
one :: a -> NonEmpty a
one x = x :| []

-- copy-pasted from @relude@
typeName :: forall a . Typeable a => String
typeName = show (typeRep @a)
