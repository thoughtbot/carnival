module Helper.Comment where

import Import
import Control.Monad
import Text.Markdown
import qualified Data.Text.Lazy as TL

data CommentRequest = CommentRequest
    { reqArticle :: Text
    , reqArticleTitle :: Text
    , reqThread :: Text
    , reqBody :: Markdown
    }

type Validated a = Either [Text] a

instance FromJSON CommentRequest where
    parseJSON (Object v) = CommentRequest
        <$> v .: "article"
        <*> v .: "articleTitle"
        <*> v .: "thread"
        <*> fmap asMarkdown (v .: "body")

        where
            asMarkdown :: TL.Text -> Markdown
            asMarkdown = Markdown . TL.filter (/= '\r')

    parseJSON _ = mzero

toComment :: UTCTime -> UserId -> CommentRequest -> Comment
toComment t uid req = Comment
    { commentUser = uid
    , commentArticleURL = reqArticle req
    , commentArticleTitle = reqArticleTitle req
    , commentThread = reqThread req
    , commentBody = reqBody req
    , commentCreated = t
    }

requireOwnComment :: UserId -> CommentId -> Handler ()
requireOwnComment uid cid = do
    c <- runDB $ get404 cid

    when (commentUser c /= uid) $
        permissionDenied "Action only appropriate for your own comments"

insertComment :: Entity User -> CommentRequest -> Handler Value
insertComment (Entity uid u) req = do
    now <- liftIO getCurrentTime

    whenValid (validateComment $ toComment now uid req) $ \c -> do
        cid <- runDB $ insert c

        sendResponseStatus status201 $ object
            ["comment" .= UserComment (Entity cid c) u]

updateComment :: CommentId -> Entity User -> CommentRequest -> Handler Value
updateComment cid (Entity uid u) req = do
    t <- fmap commentCreated $ runDB $ get404 cid

    whenValid (validateComment $ toComment t uid req) $ \c -> do
        _ <- runDB $ replace cid c

        sendResponseStatus status200 $ object
            ["comment" .= UserComment (Entity cid c) u]

validateComment :: Comment -> Validated Comment
validateComment c
    | commentBody c == "" = Left ["Body cannot be blank"]
    | otherwise = Right c

whenValid :: Validated a -> (a -> Handler Value) -> Handler Value
whenValid (Right v) f = f v
whenValid (Left es) _ = sendResponseStatus status400 $ object
    ["errors" .= (map toJSON es)]

-- N.B. This is N+1. Consider rewriting as a join, IFF this
-- becomes an issue.
addUserInfo :: Entity Comment -> YesodDB App (Maybe UserComment)
addUserInfo e@(Entity _ c) = do
    u <- get $ commentUser c
    return $ fmap (UserComment e) u
