{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Transforms the results of a fetch process in various ways,
--  creating directories, renaming files, adding metadata, etc.
--  This module serves a post-processing for fetch results.
--
--  The renaming functions observe the following policies:
--  * Characters illegal according to the local filesystem are escaped.
--  * Renaming works on a best-effort basis. If name collisions, overlong
--    path/filenames, or other IO errors occur, the affected file is skipped
--    and the process continues. At the end, the list of errors is returned.
--    __Note that in the case of name collisions, the renaming of the first
--    file may succeed; only subsequent ones will fail.@
--  * Existing files are not overwritten.
module Crawling.Hephaestos.Fetch.Transform where

import Control.Arrow
import Control.Exception
import qualified Data.Aeson as Ae
import Data.Functor.Monadic
import Data.List.Split (splitOn)
import Data.Text.Lazy (pack, Text, unpack)
import Data.Tree
import Data.Tree.Monadic
import Data.Types.Injective
import qualified Network.URI as N
import System.Directory.Generic
import System.FilePath.Generic

import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Fetch.Types
import qualified Crawling.Hephaestos.Fetch.Types.Metadata as M

readMetadata :: Text -> ErrorIO (Tree M.MetaNode)
readMetadata = undefined

-- |Renames all results to the last part of the URL's path
--  from which they were downloaded, e.g.
--  @http://domain.tld/seg1/.../segN/name?param1=arg1&...&paramM=argM@
--  becomes @name@ and stays in the same folder.
nameByURL :: Text -- ^Location of the downloaded files.
        -> Text -- ^Full name of the metadata file.
        -> ErrorIO [SomeException]
nameByURL dir metadataFile =
   readMetadata metadataFile
   >$> justLeaves id
   >>= mapErr_ (\f -> rename dir (M.metaFile f) (getPart last $ M.metaURL f))

-- |The more elaborate version of 'nameByURL' which preserves the entire path
--  of the URLs. Each part of a URL's path creates a corresponding directory.
--  E.g. @http://domain.tld/seg1/.../segN/name?param1=arg1&...&paramM=argM@
--  becomes the directory structure @domain.tld/seg1/.../segN/@. @segN@
--  contains the file @name@.
structureByURL :: Text -- ^Location of the downloaded files.
               -> Text -- ^Full name of the metadata file.
               -> ErrorIO [SomeException]
structureByURL dir metadataFile =
   readMetadata metadataFile
   >$> justLeaves id
   >>= mapErr_ (\f -> do let dir' = getPart (concat.init) $ M.metaURL f
                             new = getPart last $ M.metaURL f
                         createDirectoryIfMissing' True (dir </> dir')
                         rename (dir </> dir') (M.metaFile f) (dir </> dir' </> new))
   where
      createDirectoryIfMissing' t d = catchIO dir FileError $
                                         createDirectoryIfMissing t d

-- |Restores the original structure of the crawl tree, with the escaped URLs
--  of
--
--  __Be aware that this transformation is ''likely'' to fail, given
--  OS-imposed maximum path lengths.__
structureByTree = undefined

-- |Creates a directory structure according to a key-value-pair that was
--  downloaded ('FetchResult' of type 'Info'). A directory with the name of
--  the value will be created, and all files that are siblings or
--  descendants of siblings of the key-value-pair are put into that directory
--  The descendants may contain keys too. Of two or more keys are siblings,
--  all but the first one are ignored (and result in an error). If a node
--  has no key-value sibling, it is left as-is.
structureByKey :: Text -- ^Location of the downloaded files.
               -> Text -- ^Full name of the metadata file.
               -> Text -- ^Name of the key (e.g. "title")
               -> ErrorIO [SomeException]
structureByKey = undefined


-- Helpers
-------------------------------------------------------------------------------

-- |Tries to rename a file, failing with an exception if the file exists.
rename :: Text -- ^Directory containing the file.
       -> Text -- ^Old filename.
       -> Text -- ^New filename.
       -> ErrorIO ()
rename dir old new = doesFileExist' (dir </> old)
                     >>= \case True -> duplicateFileError
                               False -> renameFile' (dir </> old)
                                                    (dir </> new)
   where
      doesFileExist' f = catchIO f FileError (doesFileExist f)
      duplicateFileError = addNetworkError old (FileError "File already exists!")
      renameFile' o n = catchIO o FileError (renameFile o n)

-- |Gets part of a the authority and path of from an URL
getPart :: ([String] -> String) -- ^Extractor function
        -> URL
        -> Text
getPart f url = maybe url (to . f . path) . N.parseURIReference . unpack $ url
   where
      cons :: Maybe a -> [a] -> [a]
      cons a as = maybe as (:as) a
      path :: N.URI -> [String]
      path uri = cons (N.uriAuthority uri
                       >$> (\x -> map ($ x) [N.uriUserInfo, N.uriRegName, N.uriPort])
                       >$> concat)
                      (splitOn "/" $ N.uriPath uri)
