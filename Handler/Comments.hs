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
postCommentsR siteId = do
    allowCrossOrigin

    u <- requireAuth_
    t <- liftIO getCurrentTime
    c <- fmap (buildComment t u siteId) requireJsonBody

    runValidation validateComment c $ \v -> do
        userComment <- runDB $ do
            commentId <- insert v
            buildUserComment siteId (Entity commentId v) u

        let notification = NewComment userComment

        runDB $ subscribe (notificationName notification) $ entityKey u

        sendNotification notification
        sendResponseStatus status201 $ object ["comment" .= userComment]

getCommentsR :: SiteId -> Handler Value
getCommentsR siteId = do
    allowCrossOrigin

    marticle <- lookupGetParam "article"
    comments <- runDB $ findUserComments siteId marticle

    return $ object ["comments" .= comments]

optionsCommentsR :: SiteId -> Handler ()
optionsCommentsR _ = do
    allowCrossOrigin

    sendResponseStatus status200 ()

buildComment :: UTCTime -> Entity User -> SiteId -> CommentRequest -> Comment
buildComment t u siteId req = Comment
    { commentUser = entityKey u
    , commentSite = siteId
    , commentArticleURL = reqArticle req
    , commentArticleTitle = reqArticleTitle req
    , commentArticleAuthor = reqArticleAuthor req
    , commentThread = reqThread req
    , commentBody = reqBody req
    , commentCreated = t
    }
