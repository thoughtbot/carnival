module Handler.DeleteStaleComments where

import Import
import Model.Comment
import Model.Site

deleteStaleCommentsTask :: Handler ()
deleteStaleCommentsTask = do
    root <- getAppRoot
    runDB $ deleteStaleComments =<< upsertDemoSite root
