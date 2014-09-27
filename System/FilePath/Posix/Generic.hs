-- |Carbon copy of 'System.FilePath.Posix', with the
--  single difference that all occurrences of 'String'
--  have been replaced with 'Text'.
module System.FilePath.Posix.Generic where

import Control.Arrow
import qualified Data.Text as T
import qualified System.FilePath.Posix as Px

-- |Alias for 'T.Text'.
type FilePathT = T.Text

-- |Combine two paths, if the second path 'isAbsolute', then it returns the second.
combine :: FilePathT -> FilePathT -> FilePathT
(combine) x y = T.pack $ T.unpack x Px.</> T.unpack y

-- |A nice alias for 'combine'.
(</>) :: FilePathT -> FilePathT -> FilePathT
(</>) = combine

-- |Normalise a file
--
--  * // outside of the drive can be made blank
--  * / -> 'pathSeparator'
-- * ./ -> ""
normalise :: FilePathT -> FilePathT
normalise x = T.pack $ Px.normalise (T.unpack x)

-- |Is a FilePath valid, i.e. could you create a file like it?
isValid :: FilePathT -> Bool
isValid = Px.isValid . T.unpack

-- |Drop the filename.
dropFileName :: FilePathT -> FilePathT
dropFileName = T.pack . Px.dropFileName . T.unpack

-- |Split the filename into directory and file. 'combine' is the inverse.
splitFileName :: FilePathT -> (T.Text, T.Text)
splitFileName = (T.pack *** T.pack) . Px.splitFileName . T.unpack

-- |Get the filename.
takeFileName :: FilePathT -> FilePathT
takeFileName = T.pack . Px.takeFileName . T.unpack
