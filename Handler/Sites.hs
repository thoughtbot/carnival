module Handler.Sites where

import Import
import Model.Site
import Helper.Auth

import Yesod.Form.Bootstrap3

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
        Entity siteId _ <- runDB $ createSite userId site
        setMessage "Site created!"
        redirect $ SiteR siteId

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
    runDB $ destroySite siteId
    setMessage "Site deleted!"
    redirect SitesR

embedExample :: Maybe SiteId -> Widget
embedExample msiteId = do
    root <- handlerToWidget getAppRoot

    $(widgetFile "embed-example")

siteForm :: Maybe Site -> Form Site
siteForm msite = renderBootstrap3 BootstrapBasicForm $ Site
    <$> areq textField (fs "Name" "my-site") (siteName <$> msite)
    <*> areq baseUrlField (fs "Base URL" "https://example.com") (siteBaseUrl <$> msite)
    <*> areq textField (fs "Language" "en-us") (siteLanguage <$> msite)

  where
    fs :: Text -> Text -> FieldSettings site
    fs msg placeholder = FieldSettings
        { fsLabel = SomeMessage msg
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs =
            [ ("class", "form-control")
            , ("placeholder", placeholder)
            ]
        }

    baseUrlField :: Field Handler Text
    baseUrlField = flip checkM textField $ \url -> do
        n <- runDB $ count [SiteBaseUrl ==. url]

        return $ case n of
            0 -> Right url
            _ -> Left ("This URL is already in use" :: Text)

onFormSuccess :: Monad m => FormResult a -> (a -> m ()) -> m ()
onFormSuccess (FormSuccess x) f = f x
onFormSuccess _ _ = return ()
