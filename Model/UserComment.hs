module Model.UserComment where

import Import
import Model.Comment
import Model.User

import Control.Monad (forM)
import Data.List (find)
import Data.Maybe (catMaybes, maybeToList)

import Text.Markdown (Markdown(..))

import qualified Data.Text.Lazy as TL

data UserComment = UserComment (Entity Comment) (Entity User)

instance ToJSON UserComment where
    toJSON (UserComment (Entity cid c) (Entity _ u)) = object
        [ "id" .= (String $ toPathPiece cid)
        , "user_id" .= String (toPathPiece $ commentUser c)
        , "user_name" .= userName u
        , "gravatar_url" .= userGravatar u
        , "article_url" .= commentArticleURL c
        , "article_title" .= commentArticleTitle c
        , "thread" .= commentThread c
        , "body" .= unMarkdown (commentBody c)
        , "body_html" .= String (renderMarkdown c)
        ]

      where
        unMarkdown :: Markdown -> TL.Text
        unMarkdown (Markdown t) = t

findUserComments :: Maybe Text -> DB [UserComment]
findUserComments marticle = do
    let filters = maybeToList $ fmap (CommentArticleURL ==.) marticle

    selectWithUsers filters []

findRecentUserComments :: DB [UserComment]
findRecentUserComments = selectWithUsers [] [Desc CommentCreated, LimitTo 20]

selectWithUsers :: [Filter Comment] -> [SelectOpt Comment] -> DB [UserComment]
selectWithUsers filters options = do
    comments <- selectList filters options
    users <- selectList
        [ UserId <-. map (commentUser . entityVal) comments
        ] []

    fmap catMaybes $ forM comments $ \c -> do
        let userId = commentUser $ entityVal c
            muser = find ((== userId) . entityKey) users

        return $ fmap (UserComment c) muser
