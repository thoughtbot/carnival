{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Data.Text.Instances where

import Control.Monad.Error (Error(..))
import Data.Text (Text)

import qualified Data.Text as T

-- Required to use @(<|>)@ with @Either Text a@ values
instance Error Text where
    strMsg = T.pack
