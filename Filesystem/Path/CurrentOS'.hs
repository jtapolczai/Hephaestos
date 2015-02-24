{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Alias for "Filesystem.Path.CurrentOS" which adds some helpers.
module Filesystem.Path.CurrentOS' (
   module CurrentOS,
   fromText',
   toText',
   (<.>),
   -- *Escaping schemes for filenames
   Escaping,
   noEscape,
   windowsEscape,
   unixEscape,
   ) where

import Prelude hiding (FilePath)

import Control.Arrow
import Control.Applicative
import Data.Char (ord, toUpper, isSpace, intToDigit)
import Data.Functor ((<$>))
import qualified Data.Text.Lazy as T
import Filesystem.Path.CurrentOS as CurrentOS hiding ((<.>), null)

-- |An escaping scheme for the encoding of a directory or filename.
--  Be aware that:
--
--  1. the argument is treated as the name of a single file or directory. If you
--     pass in an entire path, the path separators might be escaped;
--  2. you should not escape existing paths, only ones that you want to create.
--     An escaping scheme might be overly aggressive and escape paths that the
--     filesystem would accept, which corrupts the filepath.
type Escaping = String -> String

-- |Turns a lazy Text into a FilePath.
fromText' :: T.Text -> FilePath
fromText' = fromText . T.toStrict

-- |Turns a FilePath into a lazy Text.
--  As FilePath's 'toText' might only return an approximation of the actual
--  path, this function should only be used for human-readable error messages,
--  not IO operations.
toText' :: FilePath -> T.Text
toText' = either T.fromStrict T.fromStrict . toText

-- |Synonym for addExtension
(<.>) :: FilePath -> T.Text -> FilePath
(<.>) x y = x `addExtension` T.toStrict y

-- |Escapes (with percent encoding) the characters @*/\?<>:%"$@, byte values
--  @0x00-0x1F@ (ASCII 0-31), and @0x7F@ (ASCII 127).
--  In addition, percent encoding is applied to every character of the
--  following filenames:
--
--  > CON, PRN, AUX, NUL, COM1, COM2, COM3, COM4,
--  > LPT1, LPT2, LPT3, LPT4, LST (all of these with or without any extension)
--
--  (reserved identifiers containing @$@ are avoiding via the escaping of @$@
--   itself.)
--
--  Lastly, all initial/trailing dots and whitespace characters
--  are escaped too (since Windows strips them).
--
--  'windowsEscape' is bijective:
--
--  > urlDecode . windowsEscape = id
--
--  /Note:/ this only holds if the escaped filename is shorter than the
--          filesystem's name limit and if the full path is shorter than the
--          filesystem's path limit.
--
--  If the original URL was percent-encoded, you can get back the unencoded form
--  via @urlDecode . urlDecode@ (the first application undoes 'windowsEscape',
--  the second 'urlEncode').
--
--  == Compatibility
--
--  'windowsEscape' subsumes 'unixEscape'. If portability is a concern, you should
--  therefore prefer 'windowsEscape' as an escaping scheme.
--
--  /windowsEscape is NOT identical to URL escaping. windowsEscape does not
--  escape some characters that URL escaping does (see RFC 3986)./
windowsEscape :: Escaping
windowsEscape fp | (map toUpper base) `elem` reserved = concatMap allEsc base ++ ext'
                 | otherwise = concatMap guffEsc begin
                               ++ concatMap esc main
                               ++ concatMap guffEsc end
   where
      reserved = ["CON", "PRN", "AUX", "NUL", "COM1", "COM2", "COM3", "COM4",
                  "LPT1", "LPT2", "LPT3", "LPT4", "LST"]
      (base,ext) = break ('.'==) fp
      ext' = if null ext then "" else "." ++ concatMap esc ext

      (begin, after) = span guff fp
      (end, main)    = (reverse *** reverse) . span guff . reverse $ after
      guff = (||) <$> isSpace <*> ('.'==)

      -- |Escapes @*/\?<>:%"$@, ASCII values < 32 and the ASCII value 127.
      esc '*' = "%2A"
      esc '/' = "%2F"
      esc '\\' = "%5C"
      esc '?' = "%3F"
      esc '<' = "%3C"
      esc '>' = "%3E"
      esc ':' = "%3A"
      esc '%' = "%25"
      esc '\"' = "%22"
      esc '$' = "%24"
      esc x | ord x < 32 = '%' : hex (ord x `div` 16) : hex (ord x `mod` 16) : []
            | ord x == 127 = "%7F"
            | otherwise  = [x]

      guffEsc '.' = "%2E"
      guffEsc ' ' = "%20"

      -- |Escapes every ASCII character. This should only be used for escaping
      --  elements of 'reserved'.
      allEsc x = '%' : hex (ord x `div` 16) : hex (ord x `mod` 16) : []

      hex = toUpper . intToDigit

-- |Escapes '/' and the NUL byte (ASCII 0). For general remarks, see 'windowsEscape'.
unixEscape :: Escaping
unixEscape = concatMap esc
   where
      esc '/' = "%2F"
      esc x | ord x == 0 = "%00"
            | otherwise  = [x]

-- |Doesn't do any escaping (specialisation of @id@).
noEscape :: Escaping
noEscape = id
