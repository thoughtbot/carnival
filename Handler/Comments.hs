module Handler.Comments where

import Import
import Helper.Auth
import Helper.Comment
import Helper.Request
import Data.Maybe

postCommentsR :: Handler Value
postCommentsR = do
    allowCrossOrigin

    Entity uid u <- requireAuth_

    insertComment uid u =<< requireJsonBody

optionsCommentsR :: Handler ()
optionsCommentsR = do
    allowCrossOrigin
    sendResponseStatus status200 ()

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
