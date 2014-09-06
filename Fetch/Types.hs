-- |The common types used by the other modules.
module Fetch.Types (
   URL,
   HTTPStatus,
   TextExtractor,
   InfoExtractor,
   Info,
   NetworkError (..),
   ErrorIO,
   ) where

import Control.Monad.Except
import Data.Text
import Text.XML.Cursor

-- |A URL.
type URL = String
-- |A numerical HTTP response status.
type HTTPStatus = Int

-- |A function which tries to extract content from a DOM tree.
type TextExtractor = Cursor -> Maybe Text
-- |A function tries to extract a key-value-pair from a DOM tree.
--  The first value of the result is the key, the second is the
--  value and may be absent.
type InfoExtractor = Cursor -> Info Text Text

-- |Auxiliary information that was extracted from a page
--  but isn't the primary content.
type Info k v = (k, Maybe v)

-- |The sum type of all network or file errors
--  that occur during fetching URLs or saving files locally.
data NetworkError = ConnectionError String
                    | StatusError HTTPStatus String
                    | FileError String
                    | FormatError String
                    | DataFindingError String

instance Show NetworkError where
   show (ConnectionError m) = m
   show (StatusError s m) = show s ++ ": " ++ m
   show (FileError m) = m
   show (FormatError m) = m
   show (DataFindingError m) = m

-- |An ExceptT-wrapper around IO. The type of all IO actions
--  in the program.
type ErrorIO a = ExceptT [NetworkError] IO a
