module Handler.Comments where

import Import
import Helper.Auth
import Helper.Comment
import Helper.Request
import Data.Maybe

postCommentsR :: Handler ()
postCommentsR = do
    allowCrossOrigin

    uid <- requireAuthId_
    _   <- runDB . insert . toComment uid =<< parseJsonBody_

    sendResponseStatus status201 ("CREATED" :: Text)

getCommentsR :: Handler Value
getCommentsR = do
    allowCrossOrigin

    filters  <- fmap toArticleFilter $ lookupGetParam "article"
    comments <- runDB $ mapM addUserInfo =<< selectList filters []

    return $ object ["comments" .= catMaybes comments]

    where
        toArticleFilter :: Maybe Text -> [Filter Comment]
        toArticleFilter = maybeToList . fmap (CommentArticle ==.)

        -- N.B. This is N+1. Consider rewriting as a join, IFF this
        -- becomes an issue.
        addUserInfo :: Entity Comment -> YesodDB App (Maybe UserComment)
        addUserInfo e@(Entity _ c) = do
            u <- get $ commentUser c
            return $ fmap (UserComment e) u
