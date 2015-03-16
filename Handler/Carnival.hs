module Handler.Carnival where

import Import
import Text.Coffee (coffee)
import Text.Julius (Javascript, rawJS)
import Text.Lucius (Css)

getCarnivalR :: SiteId -> Handler Javascript
getCarnivalR siteId = do
    root <- rawJS <$> getAppRoot

    let body = mconcat
            [ $(coffeeFile "embed/Article")
            , $(coffeeFile "embed/Block")
            , $(coffeeFile "embed/Comment")
            , $(coffeeFile "embed/CommentForm")
            , $(coffeeFile "embed/Indicator")
            , $(coffeeFile "embed/Thread")
            , $(coffeeFile "embed/Carnival")
            ]

    withUrlRenderer [coffee|^{body}|]

getCarnivalCssR :: SiteId -> Handler Css
getCarnivalCssR _ = withUrlRenderer $(luciusFile "embed")
