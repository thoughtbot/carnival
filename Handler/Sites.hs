module Handler.Sites where

import Import
import Language
import Model.Site
import Helper.Request

import Yesod.Form.Bootstrap3

getSitesR :: Handler Html
getSitesR = do
    Entity userId user <- requireAuth

    (widget, enctype) <- generateFormPost $ siteForm Nothing

    plan <- runDB $ belongsToJust userPlan user
    sites <- runDB $ findSites userId

    defaultLayout $ do
        setTitle "Carnival - Sites"
        $(widgetFile "sites/index")

postSitesR :: Handler Html
postSitesR = do
    Entity userId user <- requireAuth

    ((result, widget), enctype) <- runFormPost $ siteForm Nothing

    plan <- runDB $ belongsToJust userPlan user
    sites <- runDB $ findSites userId

    when (sites `overSiteQuota` plan) $ do
        setMessage =<< withUrlRenderer [hamlet|
            You're over your site quota. Please 
            <a href=@{PlansR}>upgrade your plan
            |]

        redirect SitesR

    onFormSuccess result $ \site -> do
        Entity siteId _ <- runDB $ createSite userId site
        setMessage "Site created!"
        redirect $ SiteR siteId

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

siteForm :: Maybe Site -> Form Site
siteForm msite = renderBootstrap3 BootstrapBasicForm $ Site
    <$> areq textField (fs "Name" "my-site") (siteName <$> msite)
    <*> areq baseUrlField (fs "Base URL" "https://example.com") (siteBaseUrl <$> msite)
    <*> areq languageField (fs "Language" "") (siteLanguage <$> msite <|> Just "en-us")

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
        if unchanged url
            then return $ Right url
            else ensureUnique url

      where
        unchanged url = fmap siteBaseUrl msite == Just url

        ensureUnique url = do
            n <- runDB $ count [SiteBaseUrl ==. url]

            return $ case n of
                0 -> Right url
                _ -> Left ("This URL is already in use" :: Text)

onFormSuccess :: Monad m => FormResult a -> (a -> m ()) -> m ()
onFormSuccess (FormSuccess x) f = f x
onFormSuccess _ _ = return ()

requireMemberSite :: SiteId -> Handler Site
requireMemberSite siteId = do
    userId <- requireAuthId
    fromMaybe404 $ runDB $ findMemberSite siteId userId
