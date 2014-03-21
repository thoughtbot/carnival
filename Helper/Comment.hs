module Helper.Comment where

import Import
import Control.Monad
import Yesod.Markdown
import qualified Data.Text as T

data CommentRequest = CommentRequest
    { reqThread :: Text
    , reqBody   :: Markdown
    }

instance FromJSON CommentRequest where
    parseJSON (Object v) = CommentRequest
        <$> v .: "thread"
        <*> fmap asMarkdown (v .: "body")

        where
            asMarkdown :: Text -> Markdown
            asMarkdown = Markdown . T.filter (/= '\r')

    parseJSON _ = mzero

toComment :: UserId -> CommentRequest -> Comment
toComment uid req = Comment
    { commentUser   = uid
    , commentThread = reqThread req
    , commentBody   = reqBody req
    }

requireOwnComment :: UserId -> CommentId -> Handler ()
requireOwnComment uid cid = do
    c <- runDB $ get404 cid

    when (commentUser c /= uid) $
        permissionDenied "Action only appropriate for your own comments"
