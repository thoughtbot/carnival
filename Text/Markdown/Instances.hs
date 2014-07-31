{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
--
-- This module supplies (orphan) instances for Markdown allowing it to be used
-- as a field type with persistent. These instances are also supplied in the
-- yesod-text-markdown package on Hackage, but its dependencies are such that we
-- can't install it at this time.
--
module Text.Markdown.Instances where

import Prelude

import Data.Text.Lazy (toStrict, fromStrict)
import Database.Persist.Sql
import Text.Markdown

deriving instance Eq Markdown
deriving instance Show Markdown

instance PersistField Markdown where
    toPersistValue (Markdown t) = PersistText $ toStrict t

    fromPersistValue (PersistText t) = Right $ Markdown $ fromStrict t
    fromPersistValue _ = Left "Not a PersistText value"

instance PersistFieldSql Markdown where
    sqlType _ = SqlString
