module Handler.Embed where

import Import
import Model.Site
import Helper.Request

import Text.Julius (rawJS)
import Yesod.Auth.OAuth2 (oauth2Url)

getEmbedR :: SiteId -> Handler Html
getEmbedR siteId = do
    root <- rawJS <$> getAppRoot
    branded <- runDB $ siteBranded siteId

    allowCrossOrigin
    embedLayout $ do
        toWidget $(luciusFile "embed")
        toWidget $(coffeeFile "embed/Article")
        toWidget $(coffeeFile "embed/Block")
        toWidget $(coffeeFile "embed/Comment")
        toWidget $(coffeeFile "embed/CommentForm")
        toWidget $(coffeeFile "embed/Indicator")
        toWidget $(coffeeFile "embed/Thread")
        toWidget $(coffeeFile "embed/Carnival")
        toWidget $(coffeeFile "embed/initialize")
