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

import qualified Data.Aeson as Ae

-- |Renames all downloaded Blobs to the last part of the URL's path
--  from which they were downloaded, e.g.
--  @http://domain.tld/seg1/.../segN/name?param1=arg1&...&paramM=argM@
--  becomes @name@ and stays in the same folder.
nameByURL :: Text -- ^Location of the downloaded files.
        -> Text -- ^Full name of the metadata file.
        -> ErrorIO [SomeException]
nameByURL = undefined

-- |The more elaborate version of 'nameByURL' which preserves the entire path
--  of the URLs. Each part of a URL's path creates a corresponding directory.
--  E.g. @http://domain.tld/seg1/.../segN/name?param1=arg1&...&paramM=argM@
--  becomes the directory structure @domain.tld/seg1/.../segN/@. @segN@
--  contains the file @name@.
structureByURL :: Text -- ^Location of the downloaded files.
               -> Text -- ^Full name of the metadata file.
               -> ErrorIO [SomeException]
structureByURL = undefined

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
