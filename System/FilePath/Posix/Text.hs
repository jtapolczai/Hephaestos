-- |Carbon copy of 'System.FilePath.Posix', with the
--  single difference that all occurrences of 'String'
--  have been replaced with 'Text'.
module System.FilePath.Posix.Text where

import qualified Data.Text as T
import qualified System.FilePath.Posix as Px

type FilePathT = T.Text

combine :: FilePathT -> FilePathT -> FilePathT
(combine) x y = T.pack $ T.unpack x Px.</> T.unpack y

(</>) :: FilePathT -> FilePathT -> FilePathT
(</>) = combine

normalise :: FilePathT -> FilePathT
normalise x = T.pack $ Px.normalise (T.unpack x)

isValid :: FilePathT -> Bool
isValid = Px.isValid . T.unpack
