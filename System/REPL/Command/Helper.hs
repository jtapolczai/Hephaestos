module System.REPL.Command.Helper where

import Prelude hiding (putStrLn, putStr, getLine, unwords, words, (!!), (++))
import qualified Prelude as P

import Control.Arrow (right, (+++))
import Control.Monad
import Control.Monad.State
import Data.Char (isSpace)
import Data.Either (lefts)
import Data.Either.Optional
import Data.Functor.Monadic
import qualified Data.List as L
import Data.Maybe (listToMaybe, fromJust, isNothing)
import Data.Ord
import Data.Text
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Crawling.Hephaestos.Helper.String ((++), padRight', showT)
import System.IO hiding (putStrLn, putStr, getLine)
import Text.Read (readMaybe)

first :: (a -> Bool) -> [a] -> a
first p = fromJust . L.find p

liftEM :: Monad m => (a -> m z) -> Either x1 a -> m (Either x1 z)
liftEM _ (Left a) = return $ Left a
liftEM f (Right r1) = liftM Right $ f r1

liftEM2 :: Monad m
        => (a -> b -> m z)
        -> Either x a
        -> Either x b
        -> m (Either x z)
liftEM2 f v1 v2
   | L.all optHasValue [Opt v1, Opt v2] =
      liftM Right $ getValue (liftM2 f v1 v2)
   | otherwise = return $ Left $ optValue $ first optHasValue'
                 [Opt' $ Err v1, Opt' $ Err v2]

liftEM3 :: Monad m
        => (a -> b -> c -> m z)
        -> Either x a
        -> Either x b
        -> Either x c
        -> m (Either x z)
liftEM3 f v1 v2 v3
   | L.all optHasValue [Opt v1, Opt v2, Opt v3] =
      liftM Right $ getValue (liftM3 f v1 v2 v3)
   | otherwise = return $ Left $ optValue $ first optHasValue'
                 [Opt' $ Err v1, Opt' $ Err v2, Opt' $ Err v3]

liftEM4 :: Monad m
        => (a -> b -> c -> d -> m z)
        -> Either x a
        -> Either x b
        -> Either x c
        -> Either x d
        -> m (Either x z)
liftEM4 f v1 v2 v3 v4
   | L.all optHasValue [Opt v1, Opt v2, Opt v3, Opt v4] =
      liftM Right $ getValue (liftM4 f v1 v2 v3 v4)
   | otherwise = return $ Left $ optValue $ first optHasValue'
                 [Opt' $ Err v1, Opt' $ Err v2, Opt' $ Err v3, Opt' $ Err v4]

liftEM5 :: Monad m
        => (a -> b -> c -> d -> e -> m z)
        -> Either x a
        -> Either x b
        -> Either x c
        -> Either x d
        -> Either x e
        -> m (Either x z)
liftEM5 f v1 v2 v3 v4 v5
   | L.all optHasValue [Opt v1, Opt v2, Opt v3, Opt v4, Opt v5] =
      liftM Right $ getValue (liftM5 f v1 v2 v3 v4 v5)
   | otherwise = return $ Left $ optValue $ first optHasValue'
                 [Opt' $ Err v1, Opt' $ Err v2, Opt' $ Err v3, Opt' $ Err v4,
                  Opt' $ Err v5]


(!!) :: [a] -> Int -> Maybe a
(!!) [] _ = Nothing
(!!) (x:_) 0 = Just x
(!!) (_:xs) n = xs !! (n-1)

-- |Deletes the right part of an Either.
voidR :: Either a b -> Either a ()
voidR = right $ const ()
onSucc :: Monad m => Either a b -> m (Either a c) -> m (Either a c)
onSucc (Left y) _ = return $ Left y
onSucc (Right _) x = x
