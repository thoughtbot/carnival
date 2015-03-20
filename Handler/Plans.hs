module Handler.Plans where

import Import
import Plan

getPlansR :: Handler Html
getPlansR = do
    defaultLayout $ do
        setTitle "Carnival - Plans"
        addStylesheet $ StaticR css_bootstrap_css
        $(widgetFile "plans/index")
