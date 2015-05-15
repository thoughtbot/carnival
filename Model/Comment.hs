module Model.Comment where

import Import

import Data.Time.Clock (addUTCTime)
import Text.Blaze.Html (toMarkup)
import Text.Blaze.Renderer.String

import qualified Data.Text as T

renderMarkdown :: Comment -> Text
renderMarkdown = T.pack . renderMarkup . toMarkup . commentBody

validateComment :: Validation Comment
validateComment c
    | commentBody c == "" = Left ["Body cannot be blank"]
    | otherwise = Right c

deleteStaleComments :: SiteId -> DB ()
deleteStaleComments sid = do
    now <- liftIO getCurrentTime
    let oneWeek = fromInteger $ 7 * 60 * 60 * 24
    let oneWeekAgo = addUTCTime (-oneWeek) now
    deleteWhere [CommentSite ==. sid, CommentCreated <=. oneWeekAgo]
