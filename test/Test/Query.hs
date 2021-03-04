module Test.Query
    ( querySpecs
    ) where

import Data.Aeson (Array, FromJSON (..), withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.Foldable (toList)
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
                , issueBody = ""
                , issueNumber = 2
                , issueUrl = "https://github.com/kowainik/hit-on/issues/2"
                , issueState = GH.IssueClosed
                , issueLabels = [ "CLI", "Git" ]
                , issueAssignees = [ "chshersh" ]
                }
            , Issue
                { issueTitle = "Implement basic CLI interface"
                , issueAuthorLogin = "vrom911"
                , issueBody = "`--help` option will be enough for the start"
                , issueNumber = 1
                , issueUrl = "https://github.com/kowainik/hit-on/issues/1"
                , issueState = GH.IssueClosed
                , issueLabels = [ "CLI" ]
                , issueAssignees = [ "vrom911" ]
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
          author {
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
        & set GH.statesL (GH.one GH.closed)
        & set GH.issueOrderL
            ( Just $ GH.defIssueOrder
            & set GH.fieldL GH.CreatedAt
            & set GH.directionL GH.Desc
            )
        )
        ( GH.one
        $ GH.nodes
        $  GH.title
        :| [ GH.author $ GH.one GH.login
           , GH.IssueBody
           , GH.IssueNumber
           , GH.IssueUrl
           , GH.IssueState
           , GH.IssueLabels
             $ GH.Labels
             ( GH.defLabelsArgs
             & set GH.lastL 5
             )
             (GH.nodes $ GH.one GH.LabelName)
           , GH.IssueAssignees
             $ GH.Assignees
             ( GH.defAssigneesArgs
             & set GH.lastL 5
             )
             (GH.nodes $ GH.one GH.UserLogin)
           ]
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
            },
            "body": "",
            "number": 2,
            "url": "https://github.com/kowainik/hit-on/issues/2",
            "state": "CLOSED",
            "labels": {
              "nodes": [
                {
                  "name": "CLI"
                },
                {
                  "name": "Git"
                }
              ]
            },
            "assignees": {
              "nodes": [
                {
                  "login": "chshersh"
                }
              ]
            }
          },
          {
            "title": "Implement basic CLI interface",
            "author": {
              "login": "vrom911"
            },
            "body": "`--help` option will be enough for the start",
            "number": 1,
            "url": "https://github.com/kowainik/hit-on/issues/1",
            "state": "CLOSED",
            "labels": {
              "nodes": [
                {
                  "name": "CLI"
                }
              ]
            },
            "assignees": {
              "nodes": [
                {
                  "login": "vrom911"
                }
              ]
            }
          }
        ]
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
    , issueBody        :: Text
    , issueNumber      :: Int
    , issueUrl         :: Text
    , issueState       :: GH.IssueState
    , issueLabels      :: [Text]
    , issueAssignees   :: [Text]
    } deriving stock (Show, Eq)

instance FromJSON Issue where
    parseJSON = withObject "Issue" $ \o -> do
        issueTitle       <- o .: "title"
        author           <- o .: "author"
        issueAuthorLogin <- author .: "login"
        issueBody        <- o .: "body"
        issueNumber      <- o .: "number"
        issueUrl         <- o .: "url"
        issueState       <- o .: "state"

        labels           <- o .: "labels"
        labelNodes       <- labels .: "nodes"
        issueLabels      <- parseLabels labelNodes

        assignees        <- o .: "assignees"
        assigneesNodes   <- assignees .: "nodes"
        issueAssignees   <- parseAssignees assigneesNodes

        pure Issue{..}
      where
        parseLabels :: Array -> Parser [Text]
        parseLabels = mapM (withObject "Label" $ \o -> o .: "name") . toList

        parseAssignees :: Array -> Parser [Text]
        parseAssignees = mapM (withObject "Assignee" $ \o -> o .: "login") . toList
