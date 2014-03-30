module TestHelpers.Auth
    ( authenticateAs
    ) where

import Model
import Foundation
import Yesod.Test

-- TODO: I have no idea how to do this...
authenticateAs :: User -> YesodExample App ()
authenticateAs _ = return ()
