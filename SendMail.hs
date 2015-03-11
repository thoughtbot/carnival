module SendMail
    ( sendMail

    -- Re-exports
    , Address(..)
    , Mail(..)
    ) where

import Import

import Control.Concurrent (forkIO)
import Network.Mail.Mime (Address(..), Mail(..), renderMail')
import Network.Mail.RecipientOverride (overrideRecipients)
import Network.Mail.SendGrid (sendMailWithSendGridEnv)

sendMail :: Mail -> Handler ()
sendMail m = send =<< liftIO (overrideRecipients m)

  where
    send = if appSendMail compileTimeAppSettings
        then sendViaSendGrid
        else sendToLog

sendViaSendGrid :: Mail -> Handler ()
sendViaSendGrid = void . liftIO . forkIO . sendMailWithSendGridEnv

sendToLog :: Mail -> Handler ()
sendToLog mail = do
    rendered <- liftIO $ renderMail' mail

    void $ $(logDebug) $ toStrict $ decodeUtf8 rendered
