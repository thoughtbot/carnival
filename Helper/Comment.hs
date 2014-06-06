module Helper.Comment where

import Import
import Control.Monad
import Yesod.Markdown
import qualified Data.Text as T

data CommentRequest = CommentRequest
    { reqArticle :: Text
    , reqThread  :: Text
    , reqBody    :: Markdown
    }

type Validated a = Either [Text] a

instance FromJSON CommentRequest where
    parseJSON (Object v) = CommentRequest
        <$> v .: "article"
        <*> v .: "thread"
        <*> fmap asMarkdown (v .: "body")

        where
            asMarkdown :: Text -> Markdown
            asMarkdown = Markdown . T.filter (/= '\r')

    parseJSON _ = mzero

toComment :: UTCTime -> UserId -> CommentRequest -> Comment
toComment t uid req = Comment
    { commentUser    = uid
    , commentArticle = reqArticle req
    , commentThread  = reqThread req
    , commentBody    = reqBody req
    , commentCreated = t
    }

requireOwnComment :: UserId -> CommentId -> Handler ()
requireOwnComment uid cid = do
    c <- runDB $ get404 cid

    when (commentUser c /= uid) $
        permissionDenied "Action only appropriate for your own comments"

insertComment :: UserId -> User -> CommentRequest -> Handler Value
insertComment uid u req = do
    now <- liftIO getCurrentTime

    process (validateComment $ toComment now uid req) $ \c -> do
        cid <- runDB $ insert c

        sendResponseStatus status201 $ object
            ["comment" .= UserComment (Entity cid c) u]

updateComment :: CommentId -> UserId -> User -> CommentRequest -> Handler Value
updateComment cid uid u req = do
    t <- fmap commentCreated $ runDB $ get404 cid

    process (validateComment $ toComment t uid req) $ \c -> do
        _ <- runDB $ replace cid c

        sendResponseStatus status200 $ object
            ["comment" .= UserComment (Entity cid c) u]

validateComment :: Comment -> Validated Comment
validateComment (Comment _ _ _ "" _) = Left ["Body cannot be blank"]
validateComment c = Right c

process :: Validated a -> (a -> Handler Value) -> Handler Value
process (Right v) f = f v
process (Left es) _ = sendResponseStatus status400 $ object
    ["errors" .= (map toJSON es)]
