module Handler.Feed where

import Import
import Model.User
import Model.UserComment

import Prelude (head)

import Control.Monad (when)
import Text.Blaze.Html (toMarkup)
import Yesod.RssFeed

getFeedR :: Handler RepRss
getFeedR = do
    comments <- runDB $ findRecentUserComments

    when (null comments) notFound

    feedFromComments comments

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
commentToRssEntry (UserComment (Entity _ c) (Entity _ u)) = do
    return FeedEntry
        { feedEntryLink =
            "http://robots.thoughtbot.com/" <> commentArticleURL c
        , feedEntryUpdated = commentCreated c
        , feedEntryTitle =
            "New comment from " <> userName u <> " on " <> commentArticleTitle c
        , feedEntryContent = toMarkup $ commentBody c
        }
