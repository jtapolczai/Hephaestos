{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

-- |Contains the dictionary of localized messages for the program.
module Crawling.Hephaestos.I18N (
   Msg(..),
   MsgMessage(..),
   msg,
   Lang,
   ) where

import Prelude hiding (FilePath)

import qualified Data.Text as TS
import Data.Text.Lazy
import Filesystem.Path.CurrentOS'
import Text.Shakespeare.I18N hiding (renderMessage)
import qualified Text.Shakespeare.I18N as I

data Msg = Msg

instance ToMessage Int where
   toMessage = TS.pack . show
instance ToMessage Text where
   toMessage = toStrict

mkMessage "Msg" "lang/" "en"

-- |Use this to insert localized messages.
msg :: Lang -> MsgMessage -> Text
msg x = fromStrict . I.renderMessage Msg [x]