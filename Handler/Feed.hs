module Handler.Feed where

import Import
import Prelude (head)
import Helper.Comment
import Yesod.RssFeed
import Data.Text as T (concat)
import Data.Maybe
import Control.Monad (when)
import Text.Blaze.Html (toMarkup)

getFeedR :: Handler RepRss
getFeedR = do
    comments <- runDB $ mapM addUserInfo =<< selectList [] [Desc CommentCreated, LimitTo 20]

    when (null comments) notFound

    feedFromComments $ catMaybes comments

feedFromComments :: [UserComment] -> Handler RepRss
feedFromComments comments = do
    entries <- mapM commentToRssEntry comments
    render <- getUrlRender

    rssFeedText Feed
        { feedAuthor      = "thoughtbot"
        , feedTitle       = "Carnival Comments"
        , feedDescription = "Recent comments on Carnival"
        , feedLanguage    = "en-us"
        , feedLinkSelf    = render FeedR
        , feedLinkHome    = "http://robots.thoughtbot.com"
        , feedUpdated     = getCommentCreated $ head comments
        , feedEntries     = entries
        }

    where
        getCommentCreated :: UserComment -> UTCTime
        getCommentCreated (UserComment (Entity _ c) _) =
            commentCreated c

commentToRssEntry :: UserComment -> Handler (FeedEntry (Text))
commentToRssEntry (UserComment (Entity _ c) u) = do
    return FeedEntry
        { feedEntryLink    = T.concat ["http://robots.thoughtbot.com/", commentArticleURL c]
        , feedEntryUpdated = (commentCreated c)
        , feedEntryTitle   = T.concat ["New comment from ", userName u, " on ", commentArticleTitle c]
        , feedEntryContent = toMarkup $ commentBody c
        }
