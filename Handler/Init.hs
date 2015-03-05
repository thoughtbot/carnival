module Handler.Init where

import Import
import Text.Julius

getInitR :: SiteId -> Handler Javascript
getInitR siteId = withUrlRenderer $(coffeeFile "init")
