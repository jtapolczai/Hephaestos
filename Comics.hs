{- |Provides access to all the available comics.
-}
module Comics where

import Control.Arrow
import qualified Data.Map as M

import Comics.CyanideAndHappiness
import Comics.PennyArcade
import Fetch.Types

data Comic = Comic{comicTitle::String,comicList::Manager -> ErrorIO [URL]}

comics :: M.Map String Comic
comics = M.fromList $ map (comicTitle &&& id) cs
   where
      cs = [Comic "Cyanide and Happiness" (flip cyanideList cyanideStart),
            Comic "Penny Arcade" (flip pennyArcadeList pennyArcadeStart)]
