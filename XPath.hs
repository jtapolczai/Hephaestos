-- |Wrapper around 'Text.XML' and 'Text.XML.Cursor', with some helper functions
--  added. Importing this module should be sufficient for all XPath-related
--  needs.
module XPath (
   module Text.XML,
   module Text.XML.Cursor,
   runXPath,
   at,
   )where

import Data.List (foldl')
import Text.XML (Document)
import Text.XML.Cursor

-- |Runs an XPath expression against a document tree.
runXPath :: Document -> (Cursor -> a) -> a
runXPath doc xpath = fromDocument doc $| xpath

-- |The []-predicate of XPath. Can be applied to an @Axis@
--  with @>=>@, e.g. @r $// element "div" >=> at 4@.
at :: Int -> Axis
at i = foldl' (>=>) (:[]) (replicate i followingSibling)