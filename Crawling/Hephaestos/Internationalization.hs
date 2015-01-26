{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Crawling.Hephaestos.Internationalization where

import Prelude hiding (FilePath)

import qualified Data.Text as TS
import Data.Text.Lazy
import Filesystem.Path.CurrentOS'
import Text.Shakespeare.I18N

data Messages = Messages

instance ToMessage Int where
   toMessage = TS.pack . show
instance ToMessage Text where
   toMessage = toStrict

mkMessage "Messages" "lang/" "en"
