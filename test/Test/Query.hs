module Test.Query
    ( querySpecs
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Prolens (set)
import Test.Hspec (Spec, describe, it, shouldReturn)

import Test.Data (githubGraphqlRepositoryId)

import qualified GitHub as GH


querySpecs :: GH.GitHubToken -> Spec
querySpecs token = describe "Query" $ do
    it "queries the 'github-graphql' repository id" $
        GH.queryRepositoryId token "kowainik" "github-graphql"
            `shouldReturn` githubGraphqlRepositoryId
    it "queries to latest closed issues of 'kowainik/hit-on'" $
        queryHitonIssues token `shouldReturn` Issues
            [ Issue
                { issueTitle = "Implement \"git-new\"-like command: `hit hop`"
                , issueAuthorLogin = "chshersh"
                }
            , Issue
                { issueTitle = "Implement basic CLI interface"
                , issueAuthorLogin = "vrom911"
                }
            ]

queryHitonIssues :: GH.GitHubToken -> IO Issues
queryHitonIssues token = GH.queryGitHub token (GH.repositoryToAst issuesQuery)

{-
query {
  repository(owner: "kowainik", name: "hit-on") {
    issues(last: 2, states: [CLOSED], orderBy: {field: CREATED_AT, direction: DESC}) {
        nodes {
          title
          author{
            login
          }
        }
    }
  }
}
-}
issuesQuery :: GH.Repository
issuesQuery = GH.repository
    ( GH.defRepositoryArgs
    & set GH.ownerL "kowainik"
    & set GH.nameL  "hit-on"
    )
    $ GH.one
    $ GH.issues
        ( GH.defIssuesArgs
        & set GH.lastL 2
        & set GH.statesL (GH.one GH.Closed)
        & set GH.issueOrderL
            ( Just $ GH.defIssueOrder
            & set GH.fieldL GH.CreatedAt
            & set GH.directionL GH.Desc
            )
        )
        ( GH.one
        $ GH.nodes
        $ GH.title :| [ GH.author $ GH.one GH.login ]
        )

newtype Issues = Issues
    { unIssues :: [Issue]
    } deriving stock (Show, Eq)

{- Parsing of the following JSON except the "data" field:

@
{
  "data": {
    "repository": {
      "issues": {
        "nodes": [
          {
            "title": "Implement \"git-new\"-like command: `hit hop`",
            "author": {
              "login": "chshersh"
            }
          },
          {
            "title": "Implement basic CLI interface",
            "author": {
              "login": "vrom911"
            }
          }
        ]
      }
    }
  }
}
@
-}
instance FromJSON Issues where
    parseJSON = withObject "Issues" $ \o -> do
        repository <- o .: "repository"
        issues <- repository .: "issues"
        nodes <- issues .: "nodes"

        Issues <$> mapM parseJSON nodes

data Issue = Issue
    { issueTitle       :: Text
    , issueAuthorLogin :: Text
    } deriving stock (Show, Eq)

instance FromJSON Issue where
    parseJSON = withObject "Issue" $ \o -> do
        issueTitle       <- o .: "title"
        author           <- o .: "author"
        issueAuthorLogin <- author .: "login"
        pure Issue{..}
