{-# LANGUAGE OverloadedStrings #-}

-- |Example Crawlers for a variety of use-cases.
module Crawling.Hephaestos.Crawlers.Templates (
   -- * Crawlers that generate numbered lists of links.
   fileList,
   fileList',
   -- * Crawlers for single files
   singleFile,
   -- * Crawlers for XPath-expressions
   xPathCrawler,
   -- * Crawlers that collect specific elements from pages.
   allElementsWhere,
   allElementsWhereExtension,
   allImages,
   )where

import Prelude hiding ((++))

import Control.Arrow
import Control.Exception
import Data.Char
import Data.Functor.Monadic
import qualified Data.List.Safe as L
import Data.List.Split
import Data.Maybe (mapMaybe)
import Data.ListLike (ListLike(append))
import qualified Data.Text.Lazy as T
import Data.Void
import Numeric.Peano

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Helper.String
import Crawling.Hephaestos.XPath

import Debug.Trace

-- Lists of files
-------------------------------------------------------------------------------

-- |Downloads a list of numbered files.
-- The URL must be of the form \"X\<num\>Y\" where '<num>' is a
--  decimal integer and 'Y' contains no digits. That is, '<num>' is the
--  last number in the URL.
--  The returned leaves are a list of URLs which correspond to the
--  input URL, '<num>' having been replaced by the integers in the given
--  range. If '<num>' has leading zeroes, all numbers will be padded
--  with zeroes to '<num>'\'s length.
--
-- Example:
--
-- >>> mapM_ putStrLn $ pictureList "http://domain.com/image001.jpg" [3..5]
-- http://domain.com/image003.jpg
-- http://domain.com/image004.jpg
-- http://domain.com/image005.jpg
--
-- If the URL does not contain a number, an error is returned.
-- This function does not check whether the generated URLs actually
-- exist.
fileList :: [Int] -> Successor SomeException Void
fileList range uri _ _ = case e of Nothing -> [failure]
                                   Just _ -> res
   where
      res = map (\i -> voidNode Blob $ T.pack $ before L.++ i L.++ after) indices
      failure = voidNode errorMsg (showT uri)
      errorMsg = flip Failure Nothing $ SomeException $ NetworkError (showT uri)
                 $ FormatError "URL did not contain any number."


      (b,e@(Just num),a) = getLast isNum
                           $ split (whenElt (not.isDigit))
                           $ show uri

      before = concat b
      indices = map (padLeft '0' (length num) . show) range
      after = concat a

-- |Variant of 'fileList' which finds out the begin point
--  by itself. The second parameter is the number of items
--  to download.
--  @pictureList' \"X<num>Y\" i = pictureList \"X<num>Y" [<num>..(<num>+i)]@.
fileList' :: Int -> Successor SomeException Void
fileList' num uri = fileList range uri
   where
      (_,e,_) = getLast isNum $ split (whenElt (not.isDigit)) $ show uri

      range = case e of Nothing -> []
                        Just e' -> [read e'..read e'+num-1]

-- Single files
-------------------------------------------------------------------------------

-- |Retrieves a single file as a ByteString.
singleFile :: Successor SomeException Void
singleFile uri bs _ = [voidNode (BinaryData bs) (showT uri)]

-- XPath
-------------------------------------------------------------------------------

-- |Runs an XPath-expression that returns a set of text nodes
--  against a page and returns the result set as URLs ('Blob').
--  If the given XPath-expression does not return a set of text,
--  this function returns an empty result set.
xPathCrawler :: AddReferer -> T.Text -> Successor SomeException Void
xPathCrawler t xpath = htmlSuccessor t id xPathCrawler'
   where
      xPathCrawler' uri doc _ = mapMaybe (getText
                                          >=$> combineURI uri
                                          >=$> voidNode Blob)
                                $ getXPathLeaves xpath doc


-- Specific elements
-------------------------------------------------------------------------------


-- |Searches the contents of given pairs of tags and attributes on a page.
allElementsWhere :: AddReferer
                 -> [(T.Text, T.Text)]
                    -- ^The list of tag/attribute pairs which are to be
                    --  gathered. E.g. @[("a","href"), ("img", "src")]@.
                 -> (URL -> Bool)
                    -- ^The predicate which gathered elements have
                    --  to pass.
                 -> Successor SomeException Void
allElementsWhere t tags pred = htmlSuccessor t id allImages'
   where
      allImages' uri doc _ = concatMap getRes tags
         where
            -- puts (TAG,ATTR) into an xpath-expression of the form
            -- "//TAG/@ATTR/@text()"
            -- and runs it against the given predicate
            getRes (tag, attr) =
               map (voidNode Blob . combineURI uri)
               $ filter (\x -> not ("#" `T.isPrefixOf` x) && pred x)
               $ mapMaybe getText
               $ getXPathLeaves
                 ("//" `append` tag `append` "/@" `append` attr `append` "")
                 doc

-- |Variant of 'allElementsWhere', but instead of a predicate,
--  all list of acceptable file extensions for the collected URLs
--  (e.g. @[".jpg", ".png"]@) is passed.
allElementsWhereExtension :: AddReferer
                          -> [(T.Text,T.Text)]
                          -> [T.Text]
                          -> Successor SomeException Void
allElementsWhereExtension t tags exts = allElementsWhere t tags elemOfExsts
   where
      elemOfExsts t = any (`T.isSuffixOf` stripParams t) exts

-- |Variant of 'allElementsWhere' which selects the @src@-attributes of
--  @img@-tags and the @href@-attributes of @a@-tags. Only URLs with the
--  following file extensions are returned:
--  * TIFF: @.tif, .tiff@
--  * GIF: @.gif@
--  * JPG: @.jpeg, .jpg, .jif, .jiff@
--  * JPEG 2000: @.jp2, .jpx, .j2k, .j2c@
--  * Flashpix: @.fpx@
--  * ImagePac: @.pcd@
--  * PNG: @.png@
--  * SVG: @.svg, .svgt@
allImages :: AddReferer -> Successor SomeException Void
allImages t = allElementsWhereExtension t tags exts
   where
      tags = [("img", "src"),
              ("a", "href")]

      exts = [".tif",".tiff",
              ".gif",
              ".jpeg", ".jpg", ".jif", ".jiff",
              ".jp2", ".jpx", ".j2k", ".j2c",
              ".fpx",
              ".pcd",
              ".png",
              ".svg", ".svgt"]
