module Network.Mail.SendGrid
    ( sendMailWithSendGrid
    , sendMailWithSendGridEnv
    ) where

import Prelude -- TODO: Remove when extracted

import Control.Applicative ((<$>), (<*>))
import Network.Mail.Mime (Mail(..))
import Network.Mail.SMTP (Password, UserName, sendMailWithLogin')
import Network.Socket (HostName, PortNumber(..))
import System.Environment (getEnv)

sendGridHost :: HostName
sendGridHost = "smtp.sendgrid.net"

sendGridPort :: PortNumber
sendGridPort = 587

-- | Send a @'Mail'@ with SendGrid using the given credentials
sendMailWithSendGrid :: UserName -> Password -> Mail -> IO ()
sendMailWithSendGrid = sendMailWithLogin' sendGridHost sendGridPort 

-- | Send a @'Mail'@ with SendGrid using credentials found in the
--   @SENDGRID_USERNAME@ and @SENDGRID_PASSWORD@ environment variables. Raises
--   an exception if the environment variables are not set.
sendMailWithSendGridEnv :: Mail -> IO ()
sendMailWithSendGridEnv m = do
    (u, p) <- (,)
        <$> getEnv "SENDGRID_USERNAME"
        <*> getEnv "SENDGRID_PASSWORD"

    sendMailWithSendGrid u p m
