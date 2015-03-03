module Handler.Feed where

import Import
import Model.UserComment

import Prelude (head)

import Control.Monad (when)
import Text.Blaze.Html (toMarkup)
import Yesod.RssFeed

getFeedR :: SiteId -> Handler RepRss
getFeedR siteId = do
    site <- runDB $ get404 siteId
    comments <- runDB findRecentUserComments

    when (null comments) notFound

    feedFromComments (Entity siteId site) comments

feedFromComments :: Entity Site -> [UserComment] -> Handler RepRss
feedFromComments (Entity siteId site) comments = do
    entries <- mapM (commentToRssEntry site) comments
    render <- getUrlRender

    rssFeedText Feed
        { feedAuthor      = siteRssAuthor site
        , feedTitle       = siteRssTitle site
        , feedDescription = siteRssDescription site
        , feedLanguage    = siteRssLanguage site
        , feedLinkSelf    = render $ FeedR siteId
        , feedLinkHome    = siteBaseUrl site
        , feedUpdated     = getCommentCreated $ head comments
        , feedEntries     = entries
        }

    where
        getCommentCreated :: UserComment -> UTCTime
        getCommentCreated (UserComment (Entity _ c) _) =
            commentCreated c

commentToRssEntry :: Site -> UserComment -> Handler (FeedEntry Text)
commentToRssEntry site (UserComment (Entity _ c) (Entity _ u)) =
    return FeedEntry
        { feedEntryLink = siteBaseUrl site <> commentArticleURL c
        , feedEntryUpdated = commentCreated c
        , feedEntryTitle = "New comment from "
            <> userName u
            <> " on " <> commentArticleTitle c
            <> " by " <> commentArticleAuthor c
        , feedEntryContent = toMarkup $ commentBody c
        }
