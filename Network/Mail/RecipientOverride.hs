module Network.Mail.RecipientOverride (overrideRecipients) where

import Prelude -- Remove when extracted

import Control.Applicative ((<$>))
import Network.Mail.Mime (Address(..), Mail(..))
import System.Environment (getEnvironment)

import qualified Data.Text as T

-- | Override the recipients of the given @'Mail'@ from the @EMAIL_RECIPIENTS@
--   environment variable if present. Returns the mail unmodified if not.
overrideRecipients :: Mail -> IO Mail
overrideRecipients m = do
    moverrides <- lookup "EMAIL_RECIPIENTS" <$> getEnvironment

    return $ case moverrides of
        Nothing -> m
        Just overrides -> m { mailTo = toAddresses overrides }

toAddresses :: String -> [Address]
toAddresses = map (Address Nothing) . T.splitOn "," . T.pack
