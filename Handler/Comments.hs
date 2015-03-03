module Handler.Comments where

import Import

import Model.Comment
import Model.Subscription
import Model.UserComment
import Notification

import Helper.Auth
import Helper.Request
import Helper.Validation

import Control.Monad (mzero)
import Text.Markdown

import qualified Data.Text.Lazy as TL

data CommentRequest = CommentRequest
    { reqArticle :: Text
    , reqArticleTitle :: Text
    , reqArticleAuthor :: Text
    , reqThread :: Text
    , reqBody :: Markdown
    }

instance FromJSON CommentRequest where
    parseJSON (Object v) = CommentRequest
        <$> v .: "article_url"
        <*> v .: "article_title"
        <*> v .: "article_author"
        <*> v .: "thread"
        <*> fmap asMarkdown (v .: "body")

        where
            asMarkdown :: TL.Text -> Markdown
            asMarkdown = Markdown . TL.filter (/= '\r')

    parseJSON _ = mzero

postCommentsR :: SiteId -> Handler Value
postCommentsR _ = do
    allowCrossOrigin

    u <- requireAuth_
    t <- liftIO getCurrentTime
    c <- fmap (buildComment t u) requireJsonBody

    runValidation validateComment c $ \v -> do
        cid <- runDB $ insert v

        let userComment = UserComment (Entity cid v) u
            notification = NewComment userComment

        runDB $ subscribe (notificationName notification) $ entityKey u

        sendNotification notification
        sendResponseStatus status201 $ object ["comment" .= userComment]

putCommentR :: SiteId -> CommentId -> Handler Value
putCommentR _ commentId = do
    allowCrossOrigin

    u <- requireAuth_
    c <- runDB $ get404 commentId

    requireOwnComment c $ entityKey u

    c' <- fmap (buildComment (commentCreated c) u) requireJsonBody

    runValidation validateComment c' $ \v -> do
        runDB $ replace commentId v

        sendResponseStatus status200 $ object
            ["comment" .= UserComment (Entity commentId v) u]

deleteCommentR :: SiteId -> CommentId -> Handler ()
deleteCommentR _ commentId = do
    allowCrossOrigin

    u <- requireAuth_
    c <- runDB $ get404 commentId

    requireOwnComment c $ entityKey u

    runDB $ delete commentId

    sendResponseStatus status200 ("DELETED" :: Text)

getCommentsR :: SiteId -> Handler Value
getCommentsR _ = do
    allowCrossOrigin

    marticle <- lookupGetParam "article"
    comments <- runDB $ findUserComments marticle

    return $ object ["comments" .= comments]

optionsCommentsR :: SiteId -> Handler ()
optionsCommentsR _ = do
    allowCrossOrigin

    sendResponseStatus status200 ()

buildComment :: UTCTime -> Entity User -> CommentRequest -> Comment
buildComment t u req = Comment
    { commentUser = entityKey u
    , commentArticleURL = reqArticle req
    , commentArticleTitle = reqArticleTitle req
    , commentArticleAuthor = reqArticleAuthor req
    , commentThread = reqThread req
    , commentBody = reqBody req
    , commentCreated = t
    }
