module Tasks
    ( parseTask
    ) where

import Handler.DeleteStaleComments
import Import hiding ((<>))
import Options.Applicative

type Task = Handler ()

parseTask :: IO Task
parseTask = execParser $ info (helper <*> taskParser) fullDesc

taskParser :: Parser Task
taskParser = subparser $
    command "delete-stale-comments"
        ( info
            (pure deleteStaleCommentsTask)
            (progDesc "Delete comments on the demo site older than one week")
        )
