{-# LANGUAGE OverloadedStrings #-}

-- |Example Crawlers for a variety of use-cases.
module Crawling.Hephaestos.Crawlers.Templates (
   fileList,
   fileList',
   allImages)where

import Control.Arrow
import Data.Char
import Data.Functor.Monadic
import Data.List.Split
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Void
import Numeric.Peano

import Crawling.Hephaestos.Fetch
import Crawling.Hephaestos.Fetch.Types.Successor
import Crawling.Hephaestos.Helper.String hiding ((++))
import Crawling.Hephaestos.XPath


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
fileList :: [Int] -> Successor [NetworkError] Void
fileList range url _ _ = case e of Nothing -> ([failure], [])
                                   Just _ -> (res, [])
   where
      res = map (\i -> voidNode $ Blob $ T.pack $ before ++ i ++ after) indices
      failure = voidNode $ Failure url $ [NetworkError url
                $ FormatError "URL did not contain any number."]

      (b,e@(Just num),a) = getLast isNum
                           $ split (whenElt (not.isDigit))
                           $ T.unpack url

      before = concat b
      indices = map (padLeft '0' (length num) . show) range
      after = concat a

-- |Variant of 'fileList' which finds out the begin point
--  by itself. The second parameter is the number of items
--  to download.
--  @pictureList' \"X<num>Y\" i = pictureList \"X<num>Y" [<num>..(<num>+i)]@.
fileList' :: Int -> Successor [NetworkError] Void
fileList' num url = fileList range url
   where
      (_,e,_) = getLast isNum $ split (whenElt (not.isDigit)) $ T.unpack url

      range = case e of Nothing -> []
                        Just e' -> [read e'..read e'+num-1]

-- |Retrieves all images on a page.
--  Only the contents of src-attributes in img-tags count
--  as images; background images are not.
allImages :: Successor [NetworkError] Void
allImages = htmlSuccessor id allImages'
   where
      allImages' url doc _ = (images,[])
         where
            images = mapMaybe (getText >=$> combineURL url >=$> Blob >=$> voidNode)
                     $ getXPathLeaves "//img/@src/text()" doc
