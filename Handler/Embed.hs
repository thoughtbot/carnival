module Handler.Embed where

import Import
import Helper.Request

import Text.Julius (rawJS)
import Yesod.Auth (Route(PluginR))

getEmbedR :: SiteId -> Handler Html
getEmbedR siteId = do
    root <- rawJS <$> getAppRoot

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
