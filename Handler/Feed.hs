module Handler.Feed where

import Import
import Model.UserComment

import Text.Blaze.Html (toMarkup)
import Yesod.RssFeed

getFeedR :: SiteId -> Handler RepRss
getFeedR siteId = do
    site <- runDB $ get404 siteId
    comments <- runDB $ findRecentUserComments siteId

    feedFromComments (Entity siteId site) comments

feedFromComments :: Entity Site -> [UserComment] -> Handler RepRss
feedFromComments (Entity siteId site) comments = do
    updated <- getUpdated comments
    entries <- mapM commentToRssEntry comments
    render <- getUrlRender

    rssFeedText Feed
        { feedAuthor      = siteName site
        , feedTitle       = "Comments on " <> siteName site
        , feedDescription = toHtml $ "Comments on " <> siteName site
        , feedLanguage    = siteLanguage site
        , feedLinkSelf    = render $ FeedR siteId
        , feedLinkHome    = siteBaseUrl site
        , feedUpdated     = updated
        , feedEntries     = entries
        }

    where
        getUpdated :: [UserComment] -> Handler UTCTime
        getUpdated [] = liftIO $ getCurrentTime
        getUpdated (userComment:_) = return $ userCommentCreated $ userComment

commentToRssEntry :: UserComment -> Handler (FeedEntry Text)
commentToRssEntry userComment = return FeedEntry
    { feedEntryLink = siteBaseUrl site <> "/" <> commentArticleURL comment
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
