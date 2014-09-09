-- |The common types used by the other modules.
module Fetch.Types (
   module X,
   URL,
   HTTPStatus,
   TextExtractor,
   InfoExtractor,
   Info,
   NetworkError (..),
   NetworkErrorKind (..),
   ErrorIO,
   ) where

import Control.Monad.Except
import Data.Text
import Network.HTTP.Conduit as X (HttpException(..))
import Text.XML.HXT.DOM.TypeDefs

-- |A URL.
type URL = String
-- |A numerical HTTP response status.
type HTTPStatus = Int

-- |A function which tries to extract content from a DOM tree.
type TextExtractor = XmlTree -> Maybe Text
-- |A function tries to extract a key-value-pair from a DOM tree.
--  The first value of the result is the key, the second is the
--  value and may be absent.
type InfoExtractor = XmlTree -> Info Text Text

-- |Auxiliary information that was extracted from a page
--  but isn't the primary content.
type Info k v = (k, Maybe v)

-- |A network error, consisting of a 'NetworkErrorKind' and
--  a URL indicating the error's source (if any).
data NetworkError = NetworkError URL NetworkErrorKind

instance Show NetworkError where
   show (NetworkError url kind) = "Error in '" ++ url ++ "': " ++ show kind

-- |The sum type of all network or file errors
--  that occur during fetching URLs or saving files locally.
data NetworkErrorKind = HttpError HttpException
                        | FileError String
                        | FormatError String
                        | DataFindingError String
                        | HTMLParsingError


instance Show NetworkErrorKind where
   show (HttpError m) = show m
   show (FileError m) = m
   show (FormatError m) = m
   show (DataFindingError m) = m
   show (HTMLParsingError) = "Couldn't parse file as HTML!"

-- |An ExceptT-wrapper around IO. The type of all IO actions
--  in the program.
type ErrorIO a = ExceptT [NetworkError] IO a
