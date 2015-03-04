module Handler.Sites where

import Import
import Model.Site
import Helper.Auth

import Control.Monad (void)

getSitesR :: Handler Html
getSitesR = do
    userId <- requireAuthId

    (widget, enctype) <- generateFormPost $ siteForm Nothing

    sites <- runDB $ findSites userId

    defaultLayout $ do
        setTitle "Carnival - Sites"
        $(widgetFile "sites/index")

postSitesR :: Handler Html
postSitesR = do
    userId <- requireAuthId

    ((result, widget), enctype) <- runFormPost $ siteForm Nothing

    onFormSuccess result $ \site -> do
        void $ runDB $ createSite userId site
        setMessage "Site created!"
        redirect $ SitesR

    sites <- runDB $ findSites userId

    defaultLayout $ do
        setTitle "Carnival - Sites"
        $(widgetFile "sites/index")

getSiteR :: SiteId -> Handler Html
getSiteR siteId = do
    site <- requireMemberSite siteId

    (widget, enctype) <- generateFormPost $ siteForm $ Just site

    defaultLayout $ do
        setTitle "Carnival - Site"
        $(widgetFile "sites/show")

postSiteR :: SiteId -> Handler Html
postSiteR siteId = do
    site <- requireMemberSite siteId

    ((result, widget), enctype) <- runFormPost $ siteForm $ Just site

    onFormSuccess result $ \site' -> do
        void $ runDB $ replace siteId site'
        setMessage "Site updated!"
        redirect $ SiteR siteId

    defaultLayout $ do
        setTitle "Carnival - Site"
        $(widgetFile "sites/show")

getDeleteSiteR :: SiteId -> Handler Html
getDeleteSiteR siteId = do
    site <- requireMemberSite siteId

    defaultLayout $ do
        setTitle "Carnival - Delete site"
        $(widgetFile "sites/delete")

postDeleteSiteR :: SiteId -> Handler ()
postDeleteSiteR siteId = do
    void $ requireMemberSite siteId
    runDB $ destroySite $ siteId
    setMessage "Site deleted!"
    redirect SitesR

siteForm :: Maybe Site -> Form Site
siteForm msite = renderDivs $ Site
    <$> areq textField "Name" (siteName <$> msite)
    <*> areq textField "Base URL" (siteBaseUrl <$> msite)
    <*> areq textField "RSS Author" (siteRssAuthor <$> msite)
    <*> areq textField "RSS Title" (siteRssTitle <$> msite)
    <*> areq htmlField "RSS Description" (siteRssDescription <$> msite)
    <*> areq textField "RSS Language" (siteRssLanguage <$> msite)

onFormSuccess :: Monad m => FormResult a -> (a -> m ()) -> m ()
onFormSuccess (FormSuccess x) f = f x
onFormSuccess _ _ = return ()
