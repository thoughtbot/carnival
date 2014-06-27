module Handler.Comments where

import Import
import Helper.Auth
import Helper.Comment
import Helper.Request
import Data.Maybe

postCommentsR :: Handler Value
postCommentsR = do
    allowCrossOrigin

    u <- requireAuth_

    insertComment u =<< requireJsonBody

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
