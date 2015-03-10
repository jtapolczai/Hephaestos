module Crawling.Hephaestos.Fetch.Types.RequestConfig (
   RequestConfig(..),
   method,
   secure,
   requestHeaders,
   redirectCount,
   requestTimeout,
   requestCookies,
   runRequestConfig,
   ) where

import Control.Applicative
import Control.Lens (makeLenses, (^.), (%~), (&))
import Data.Default
import Data.Monoid
import qualified Data.List.Safe as L
import qualified Data.Set as S
import qualified Network.HTTP.Conduit as C
import qualified Network.HTTP.Types as C

newtype FstComparer a b = FstC{unFstC::(a,b)}

instance Eq a => Eq (FstComparer a b) where FstC (a,_) == FstC (b,_) = a == b
instance Ord a => Ord (FstComparer a b) where
   compare (FstC (a,_)) (FstC (b,_)) = compare a b

-- |Represents a modification of a request-
--  Configurations are monoids and can be chained.
data RequestConfig = RequestConfig {_method :: Maybe C.Method,
                                    _secure :: Maybe Bool,
                                    _requestHeaders :: S.Set C.Header,
                                    _redirectCount :: Maybe Int,
                                    _requestTimeout :: Maybe (Maybe Int),
                                    _requestCookies :: [C.Cookie]}
   deriving (Show, Eq, Read)

makeLenses ''RequestConfig

-- |Overwrites everything of the left operand, except for headers and cookies.
--  For the cookies, both lists are concatenated. For the headers, the
--  right-biased union is taken (headers of the second operand overwrite
--  headers from the first one).
--
--  'RequestConfig' has no right identity, but its 'Default' instance is
--  a left identitiy.
instance Monoid RequestConfig where
   mempty = RequestConfig Nothing Nothing S.empty Nothing Nothing []
   mappend from to = to & method %~ (<|> (from ^. method))
                        & secure %~ (<|> (from ^. secure))
                        & redirectCount %~ (<|> (from ^. redirectCount))
                        & requestTimeout %~ (<|> (from ^. requestTimeout))
                        & requestHeaders %~ (`union'` (from ^. requestHeaders))
                        & requestCookies %~ (++       (from ^. requestCookies))
      where
         -- The argument order is important because union is left-biased.
         union' x y = S.map unFstC $ S.union (S.map FstC x) (S.map FstC y)

-- |A thing wrapper around the Default instance of 'C.Request'. Changes are:
--
--  * The timeout is 40 seconds.
--
--  Note that 'def' is not the same as 'mempty'. 'mempty' applies no changes,
--  'def' sets the relevant fields to their default values.
instance Default RequestConfig where
   def = RequestConfig
         {_method = Just (C.method req),
          _secure = Just (C.secure req),
          _requestHeaders = S.fromList (C.requestHeaders req),
          _redirectCount = Just (C.redirectCount req),
          _requestTimeout = Just (Just 40),
          _requestCookies = []
         }
      where req :: C.Request
            req = def

-- |Turns a 'RequestConfig' into a function that modifies requests.
--  Use this function to \'apply\' a 'RequestConfig' to a 'C.Request'.
runRequestConfig :: RequestConfig -> C.Request -> C.Request
-- This looks like garbage, but since lenses are not available for C.Request,
-- the update has to be hacked together in this way.
runRequestConfig conf req =
   maybe id (\x r -> r{C.redirectCount = x}) (conf ^. redirectCount)
   . maybe id (\x r -> r{C.secure = x}) (conf ^. secure)
   . maybe id (\x r -> r{C.method = x}) (conf ^. method)
   $ req{C.requestHeaders = L.unionBy (eq fst) (S.toList (conf ^. requestHeaders))
                                               (C.requestHeaders req),
         C.cookieJar = Just (cookieUnion (C.cookieJar req) (conf ^. requestCookies))}
      where
         cookieUnion Nothing y = C.createCookieJar y
         cookieUnion (Just x) y = C.createCookieJar $ C.destroyCookieJar x L.++ y

         eq :: Eq b => (a -> b) -> a -> a -> Bool
         eq f x y = f x == f y
