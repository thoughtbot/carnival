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

-- | Updates the given comment with the request data, preserving the comment's
--   created-at. Will respond 404 if the comment is not present.
updateComment :: CommentId -> UserId -> CommentRequest -> YesodDB App ()
updateComment cid uid req = do
    t <- fmap commentCreated $ get404 cid
    _ <- replace cid $ toComment t uid req
    return ()
