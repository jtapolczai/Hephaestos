{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- |Carbon copy of 'System.FilePath', with the
--  single difference that all occurrences of 'String'
--  have been replaced with 'Text'.
module System.FilePath.Generic where

import Control.Arrow
import Data.Types.Isomorphic
import qualified System.FilePath as Px


-- |Combine two paths, if the second path 'isAbsolute', then it returns the second.
combine :: (Iso a String)
        => a -> a -> a
(combine) x y = to $ to x Px.</> to y

-- |A nice alias for 'combine'.
(</>) :: (Iso a String)
      => a -> a -> a
(</>) = combine

-- |Normalise a file
--
--  * // outside of the drive can be made blank
--  * / -> 'pathSeparator'
-- * ./ -> ""
normalise :: (Iso a String) => a -> a
normalise = to . Px.normalise . to

-- |Is a FilePath valid, i.e. could you create a file like it?
isValid :: (Injective a String) => a -> Bool
isValid = Px.isValid . to

-- |Drop the filename.
dropFileName :: (Iso a String) => a -> a
dropFileName = to . Px.dropFileName . to

-- |Split the filename into directory and file. 'combine' is the inverse.
splitFileName :: (Iso a String)
              => a -> (a, a)
splitFileName = (to *** to) . Px.splitFileName . to

-- |Get the filename.
takeFileName :: (Iso a String) => a -> a
takeFileName = to . Px.takeFileName . to
