module Debug where

import Fetch
import Fetch.Tree
import Fetch.Mapping

import Galleries.Simple
import Galleries.Retrieval
import Galleries.List
import Galleries.Tree
import Galleries.Porn


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

