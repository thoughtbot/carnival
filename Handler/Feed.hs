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
    comments <- runDB $ findRecentUserComments siteId

    when (null comments) notFound

    feedFromComments (Entity siteId site) comments

feedFromComments :: Entity Site -> [UserComment] -> Handler RepRss
feedFromComments (Entity siteId site) comments = do
    entries <- mapM commentToRssEntry comments
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
        getCommentCreated = commentCreated . entityVal . userCommentComment

commentToRssEntry :: UserComment -> Handler (FeedEntry Text)
commentToRssEntry userComment = return FeedEntry
    { feedEntryLink = siteBaseUrl site <> commentArticleURL comment
    , feedEntryUpdated = commentCreated comment
    , feedEntryTitle = "New comment from "
        <> userName user
        <> " on " <> commentArticleTitle comment
        <> " by " <> commentArticleAuthor comment
    , feedEntryContent = toMarkup $ commentBody comment
    }

  where
    comment = entityVal $ userCommentComment userComment
    site = entityVal $ userCommentSite userComment
    user = entityVal $ userCommentUser userComment
