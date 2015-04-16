module IntegrationImport
    ( inModal
    , clickOn
    , fillIn
    , waitForElem
    , waitForText
    , module X
    ) where

import ClassyPrelude            as X hiding (Element)
import Test.Hspec.WebDriver     as X hiding (shouldBe, shouldReturn)

import Test.WebDriver.Class
import Test.WebDriver.Commands.Wait

-- N.B. relies on the assumption that a modal has been opened and is the only
-- window open besides the current one.
inModal :: WebDriver m => m a -> m ()
inModal f = do
    cw <- getCurrentWindow
    focusWindow =<< return . unsafeHead . filter (/= cw) =<< windows
    void $ f
    focusWindow cw

clickOn :: WebDriver m => Selector -> m ()
clickOn = click <=< waitForElem

fillIn :: WebDriver m => Selector -> Text -> m Element
fillIn x value = do
    input <- findElem x
    sendKeys value input
    return input

waitForText :: WebDriver m => Selector -> m Text
waitForText = getText <=< waitForElem

waitForElem :: WebDriver m => Selector -> m Element
waitForElem = waitUntil timeout . findElem
  where
    timeout = 2000
