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
import Data.ListLike (ListLike(append), StringLike(fromString))
import Data.Maybe (mapMaybe, fromJust)
import qualified Data.Text.Lazy as T
import Data.Void
import Numeric.Peano

import Crawling.Hephaestos.Crawlers.Utils
import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor

import Debug.Trace

-- Lists of files
-------------------------------------------------------------------------------

-- |Downloads a list of numbered files.
--  Given a list of integers @[i_1,...,i_n]@ URL of the form @XmY@,
--  where @m@ is the last decimal number in the
--  URL, this function generates the Blobs
--
-- @
--  Xi_1Y
--  Xi_2Y
--  ...
--  Xi_nY
-- @
--
-- Thus, @m@ only signifies where the "hole" is which is to be filled in. It
-- is thrown away. However, if there is an @i_j@ s.t. @i_j = m@, then the node
-- for "Xi_jY" will be of type 'BinaryData' instead of 'Blob', since that URL's
-- contents have already been downloaded.
--
-- If the given URL is not of the form @XmY@, then a single 'Failure' noe is
-- generated.
--
-- This function does not check whether the generated URLs actually
-- exist.
fileList :: [Int] -> Successor SomeException Void Void
fileList range uri _ _ = return $ case e of Nothing -> [noNum]
                                            Just _ -> map f indices
   where
      noNum = voidNode $ failure (URLHadNoNumberError $ fromString $ show uri) Nothing

      fillIn (x,Just y,z) i = voidNode
                              $ makeLink uri blob
                              $ T.pack $ concat x L.++ i L.++ concat z

      (f, e@(Just num)) = fillIn &&& (\(_,y,_) -> y)
                          $ getLast isNum
                          $ split (whenElt (not.isDigit))
                          $ show uri

      indices = map (padLeft '0' (length num) . show) range

      padLeft :: a -> Int -> [a] -> [a]
      padLeft c i cs = replicate (i - length cs) c L.++ cs

-- |Variant of 'fileList' which finds out the range by itself.
--  The second parameter is the number of items to download.
--
-- @
--  pictureList' \"XmY\" i = pictureList \"XmY\" [m..(m+i)]
-- @
fileList' :: Int -> Successor SomeException Void Void
fileList' num uri = fileList range uri
   where
      (_,e,_) = getLast isNum $ split (whenElt (not.isDigit)) $ show uri

      range = case e of Nothing -> []
                        Just e' -> [read e'..read e'+num-1]

-- |Returns True iff the string is composed only of digits and is not empty.
isNum :: String -> Bool
isNum x = all isDigit x && not (null x)

-- |Gets the last element of a list which fulfils a given predicate.
--  The elements of the list before and after that element are also
--  returned. Only works for finite lists.
--  @any f xs == True@ implies @getLast f xs == (ys,Just e,zs)@
--  such that @xs == ys ++ [e] ++ zs@, @f e == True@ and @any f zs == False@.
--  On the other hand, @any f xs == False@ implies
--  @getLast f xs == ([],Nothing,xs)@.
getLast :: (a -> Bool) -> [a] -> ([a],Maybe a,[a])
getLast f xs = (concat $ L.init before, L.last before, after xs)
   where
      after = reverse . takeWhile (not . f) . reverse
      before = take (length xs - length (after xs)) xs

-- Single files
-------------------------------------------------------------------------------

-- |Retrieves a single file as a ByteString.
singleFile :: Successor SomeException Void Void
singleFile uri bs _ = bs >$> \bs' -> [voidNode $ binaryData bs']

-- XPath
-------------------------------------------------------------------------------

-- |Runs an XPath-expression that returns a set of text nodes
--  against a page and returns the result set as URLs ('Blob').
--  If the given XPath-expression does not return a set of text,
--  this function returns an empty result set.
xPathCrawler :: T.Text -> Successor SomeException Void Void
xPathCrawler xpath = htmlSuccessor id xPathCrawler'
   where
      xPathCrawler' uri doc _ = return
                                $ mapMaybe (getText
                                            >=$> makeLink uri blob
                                            >=$> voidNode)
                                $ getXPath xpath doc


-- Specific elements
-------------------------------------------------------------------------------


-- |Searches the contents of given pairs of tags and attributes on a page.
allElementsWhere :: [(T.Text, T.Text)]
                    -- ^The list of tag/attribute pairs which are to be
                    --  gathered. E.g. @[("a","href"), ("img", "src")]@.
                 -> (URL -> Bool)
                    -- ^The predicate which gathered elements have
                    --  to pass.
                 -> Successor SomeException Void Void
allElementsWhere tags pred = htmlSuccessor id allWhere'
   where
      allWhere' uri doc _ = return $ concatMap getRes tags
         where
            -- puts (TAG,ATTR) into an xpath-expression of the form
            -- "//TAG/@ATTR/@text()"
            -- and runs it against the given predicate
            getRes (tag, attr) =
               map (voidNode . makeLink uri blob)
               $ filter (\x -> not ("#" `T.isPrefixOf` x) && pred x)
               $ mapMaybe getText
               $ getXPath
                 ("//" `append` tag `append` "/@" `append` attr `append` "")
                 doc

-- |Variant of 'allElementsWhere', but instead of a predicate,
--  all list of acceptable file extensions for the collected URLs
--  (e.g. @[".jpg", ".png"]@) is passed.
allElementsWhereExtension :: [(T.Text,T.Text)]
                          -> [T.Text]
                          -> Successor SomeException Void Void
allElementsWhereExtension tags exts = allElementsWhere tags elemOfExsts
   where
      elemOfExsts t = any (`T.isSuffixOf` stripParams t) exts

      stripParams = T.takeWhile ('?'/=)

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
allImages :: Successor SomeException Void Void
allImages = allElementsWhereExtension tags exts
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
