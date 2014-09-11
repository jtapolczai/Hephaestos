{- |Provides access to all the available comics.
-}
module Comics where

import Control.Arrow
import qualified Data.Map as M
import Data.Text (Text)

import Comics.LinearComic
import Comics.List
import Fetch.Types

comics :: M.Map Text LinearComic
comics = M.fromList $ map (comicName &&& id)
         [xkcd,
         pennyArcade,
         cyanideAndHappiness]

