{- |
Copyright: (c) 2021 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Data types and helper functions to work with @title@.
-}

module GitHub.Title
    ( HasTitle (..)
    ) where

import GitHub.Issue (IssueField (..))
import GitHub.PullRequests (PullRequestField (..))


{- | Typeclass for objects that have a @title@ field.
-}
class HasTitle field where
    title :: field

instance HasTitle IssueField where
    title :: IssueField
    title = IssueTitle

instance HasTitle PullRequestField where
    title :: PullRequestField
    title = PullRequestTitle
