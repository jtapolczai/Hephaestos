{-# LANGUAGE FlexibleContexts #-}
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
module Crawling.Hephaestos.Transform (
   TransformationName(..),
   Transformation(..),
   getTransformation,
   nameByURL,
   structureByURL,
   structureByKey,
   ) where

import Prelude hiding (FilePath)

import Control.Applicative
import Control.Arrow
import Control.Monad (join)
import Control.Monad.Catch
import Control.Monad.IO.Class
import qualified Data.Aeson as Ae
import qualified Data.ByteString.Lazy as BL
import Data.Functor.Monadic
import Data.List.Safe (foldl')
import Data.List.Split (splitOn)
import Data.ListLike (ListLike(append), StringLike(fromString))
import Data.Maybe (catMaybes, fromJust)
import Data.Text.Lazy (pack, Text, unpack, fromStrict, toStrict)
import Data.Tree
import Data.Tree.Monadic
import Data.Types.Injective
import Data.Void
import qualified Filesystem.Path.CurrentOS' as Fp
import qualified Network.URI as N
import qualified System.Directory as D
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))

import Crawling.Hephaestos.Fetch.ErrorHandling
import Crawling.Hephaestos.Fetch.Types
import Crawling.Hephaestos.Fetch.Successor(HasExt(..))
import qualified Crawling.Hephaestos.Metadata as M

-- |Takes a directory name, the name of a metadata file,
--  and performs a transformation on the files in the given directory,
--  returning the list of errors which occurred.
type Transformation = Fp.FilePath -> Fp.FilePath -> IO [SomeException]

data TransformationName = NameByURL | StructureByURL | StructureByKey | TransID
   deriving (Eq, Ord, Enum, Show, Read, Bounded)

instance Pretty TransformationName where
   pPrint NameByURL = PP.text "name by URL: get the filenames from the URLs"
   pPrint StructureByURL = PP.text "structure by URL: organize according to URL structure"
   pPrint StructureByKey = PP.text "structure by key: organize according to \"title\" keys"
   pPrint TransID = PP.text "do nothing"

-- |Gets the transformation associated with a name.
getTransformation :: TransformationName -> Transformation
getTransformation NameByURL = nameByURL
getTransformation StructureByURL = structureByURL
getTransformation StructureByKey = structureByKey'
getTransformation TransID = \_ _ -> return []

-- |Renames all results to the last part of the URL's path
--  from which they were downloaded, e.g.
--  @http://domain.tld/seg1/.../segN/name?param1=arg1&...&paramM=argM@
--  becomes @name@ and stays in the same folder.
-- |Renames all results to the last part of the URL's path
--  from which they were downloaded, e.g.
--  @http://domain.tld/seg1/.../segN/name?param1=arg1&...&paramM=argM@
--  becomes @name@ and stays in the same folder.
nameByURL :: Transformation
nameByURL dir metadataFile =
   M.readMetadata' metadataFile
   >$> urlsToLeaves
   >>= mapErr_ (\f -> maybe (throwM $ dataFormatError' $ leafURL f)
                            (\x -> do old <- getFileName dir f >$> Fp.fromText'
                                      rename dir old x)
                            (getPart (Fp.decodeString.last) $ leafURL f))

-- |The more elaborate version of 'nameByURL' which preserves the entire path
--  of the URLs. Each part of a URL's path creates a corresponding directory.
--  E.g. @http://domain.tld/seg1/.../segN/name?param1=arg1&...&paramM=argM@
--  becomes the directory structure @domain.tld/seg1/.../segN/@. @segN@
--  contains the file @name@.
structureByURL :: Transformation
structureByURL dir metadataFile =
   M.readMetadata' metadataFile
   >$> urlsToLeaves
   >>= mapErr_ renameWithDir
   where
      renameWithDir f = do
         let mdir = getPart (foldl' (Fp.</>) dir . map Fp.decodeString . init) $ leafURL f
             mnew = getPart (Fp.decodeString . last) $ leafURL f
         case (mdir,mnew) of
            (Just dir', Just new) -> do
               D.createDirectoryIfMissing True (Fp.encodeString dir')
               old <- getFileName dir f >$> Fp.fromText'
               rename dir old (dir' Fp.</> new)
            _ -> throwM $ dataFormatError' $ leafURL f

-- |Creates a directory structure according to a key-value-pair that was
--  downloaded ('FetchResult' of type 'Info'). A directory with the name of
--  the value will be created, and all files that are siblings or
--  descendants of siblings of the key-value-pair are put into that directory
--  The descendants may contain keys too. If two or more keys are siblings,
--  they are all ignored (and an error is reported). If a node
--  has no key-value sibling, or if reading /any/ of its sibling which are keys
--  results in an error, the node is left as-is.
structureByKey :: Text -- ^Location of the downloaded files.
               -> Transformation
structureByKey key dir metadataFile =
   M.readMetadata' metadataFile
   >>= keyTransform []
   -- Perform renamings and concatenate the errors from keyTransform and rename
   >$> first (mapErr_ $ \(o,n) -> do old <- getFileName dir o >$> Fp.fromText'
                                     rename dir old n)
   >>= (\(m,e) -> (++) <$> m <*> return e)
   where
      keyTransform :: [Fp.FilePath] -> Tree (M.MetaNode i) -> IO ([(M.MetaNode i, Fp.FilePath)], [SomeException])
      keyTransform d (Node n xs) = do
         titles <- (filter (M.isInfo . M.metaType . rootLabel) xs
                    |> mapM (getKey key . M.metaFile . rootLabel)
                    >$> catMaybes
                    >$> map Fp.fromText'
                    >$> Right) `catch` (return . Left)
         case titles of
            Right [] -> mapM (keyTransform d) xs >$> unzip >$> (concat *** concat)
            Right [t] -> mapM (keyTransform (d++[t])) xs >$> unzip >$> (concat *** concat)
            Right _ -> return ([], [SomeException $ AmbiguousDataError "More than one key found."])
            Left e -> return ([],[e])

      getKey :: Text -> Text -> IO (Maybe Text)
      getKey keyName file = do
         contents <- readFile (unpack file)
         let key = pack $ head $ lines contents
             value = unlines $ tail $ lines contents
         return $ if key == keyName then Just $ pack value
                                    else Nothing


-- |Variant of 'structureByKey' that sets the key name to "title".
structureByKey' :: Transformation
structureByKey' = structureByKey "title"

-- Helpers
-------------------------------------------------------------------------------

-- |Gets the filename of a result from a leaf.
--  For error files, the most recent one will be returned.
getFileName :: (MonadThrow m, MonadIO m) => Fp.FilePath -> M.MetaNode i -> m Text
getFileName dir (M.Leaf file ty _ _) = do
   lastFile <- liftIO $ takeWhileM D.doesFileExist files
   case lastFile of [] -> throwM $ DataMissingError "" $ Just "No file corresponding to the name in the metadata exists!"
                    xs -> return $ pack $ last xs
   where
      files = map (Fp.encodeString . (dir Fp.</>))
              $ (Fp.fromText' file Fp.<.> ext ty) : map numberFile [1..]

      numberFile :: Int -> Fp.FilePath
      numberFile i = Fp.decodeString (unpack file `append` show i) Fp.<.> ext ty

      takeWhileM :: (Functor m, Monad m) => (a -> m Bool) -> [a] -> m [a]
      takeWhileM _ [] = return []
      takeWhileM f (x:xs) = f x >>= \case False -> return []
                                          True -> (x:) <$> takeWhileM f xs

-- |Gets a part of the authority and path of an URL.
getPart :: ([String] -> a) -- ^Extractor function
        -> URL
        -> Maybe a
getPart f url = f . path <$< (N.parseURIReference . unpack $ url)
   where
      cons :: Maybe a -> [a] -> [a]
      cons a as = maybe as (:as) a
      path :: N.URI -> [String]
      path uri = cons (N.uriAuthority uri
                       >$> (\x -> map ($ x) [N.uriUserInfo, N.uriRegName, N.uriPort])
                       >$> concat)
                      (splitOn "/" $ N.uriPath uri)

-- |Gets the leaves from a tree of metanodes, and puts the URL of a leaf's parent
--  into the leav. \"Leaf\", in this context, means the 'M.Leaf' constructor of
--  'M.MetaNode'. Inner nodes which merely have no children are not returned by
--  this function. If a leaf has no parent (this should not occur), it is given
--  the empty string as URL.
--
--  Virtually any transformation that needs the URL from which data was fetched
--  needs to run this function over the tree. While the leaves can be extracted
--  from the tree of MetaNodes directly, 'urlsToLeaves' does it is a standard,
--  safe way.
urlsToLeaves :: Tree (M.MetaNode i) -> [M.MetaNode i]
urlsToLeaves = catMaybes . leaves leafF innerF ""
   where
      leafF l@M.Leaf{M.metaLeafURL=Just _} _ = Just l
      leafF l@M.Leaf{M.metaLeafURL=Nothing} url = Just $ l{M.metaLeafURL = Just url}
      leafF l@M.InnerNode{} _ = Nothing

      innerF (M.InnerNode url) _ = url

-- |Synonym for @fromJust . metaLeafURL
leafURL = fromJust . M.metaLeafURL

-- |Tries to rename a file, failing with an exception if the file exists.
rename :: Fp.FilePath -- ^Directory containing the file.
       -> Fp.FilePath -- ^Old filename.
       -> Fp.FilePath -- ^New filename.
       -> IO ()
rename dir old new =
   D.doesFileExist (Fp.encodeString new')
   >>= \case True -> throwM $ duplicateFileError (fromString oldS) (fromString newS)
             False -> D.renameFile oldS newS
   where
      old' = dir Fp.</> old
      new' = dir Fp.</> new

      newS = Fp.encodeString new'
      oldS = Fp.encodeString old'
