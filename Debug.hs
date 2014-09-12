module Debug where

import Fetch
import Fetch.Iterating
import Fetch.Mapping

import Comics.CyanideAndHappiness
import Comics.PennyArcade
import Comics.EightMuses

import Galleries.Simple
import Galleries.Retrieval

import System.IO.Unsafe
import Data.ByteString.Lazy
import XPath
import Control.Monad.Except
import Control.Arrow

bruteDownload :: URL -> ByteString
bruteDownload = unsafePerformIO . simpleDownload

bruteDoc :: URL -> XmlTree
bruteDoc u = fr $ unsafePerformIO $ runExceptT $ toDocument u (bruteDownload u)
   where fr (Right r) = r