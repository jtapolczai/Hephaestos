{-# LANGUAGE OverloadedStrings #-}

-- |Small helper functions relating to strings.
module Crawling.Hephaestos.Helper.String where

import Prelude hiding ((++))
import qualified Prelude as P

import Data.Char (isDigit)
import Data.List
import Data.List.Split
import qualified Data.Text as T
import qualified System.FilePath as Px

-- |Strips relative parts of a path from its beginning.
--  Specifically, it removes @../@ and @/@ from the beginning
--  of the string.
stripRel :: T.Text -> T.Text
stripRel xs | T.take 3 xs == "../" = stripRel $ T.drop 3 xs
            | T.take 1 xs == "/"   = stripRel $ T.drop 1 xs
            | otherwise = xs

-- |Takes x and y and, if y begins with @../@, returns
--  @x ++ stripRel y@. Otherwise, it returns @y@.
appendAbs :: T.Text -> T.Text -> T.Text
appendAbs x y = if T.take 3 y == "../" || T.head y == '/' then x `T.append` stripRel y else y


-- |Appends one URL to another. If the second URL
--  is absolute (i.e. it starts with \"http://\"), the
--  first one is ignored. The resultant URL is normalized
--  on a best-effort basis: whenever the segment
--  @/X/../@ occurs, it is removed. This works recursively.
combineURL :: T.Text -> T.Text -> T.Text
combineURL x y = T.pack $ intercalate "/" $ normalize $ xs P.++ ys
   where (x',_) = break ('?'==) $ T.unpack x
         xs = filter (not.null) $ splitOn "/" x'
         ys = filter (not.null) $ splitOn "/" $ T.unpack y

         normalize zs = if null rest then zs
                        else normalize $ init' zs' P.++ tail rest
            where (zs',rest) = break (".."==) zs

         init' [] = []
         init' (l:ls) = init (l:ls)

-- |Gets the last element of a list which fulfils a given predicate.
--  The elements of the list before and after that element are also
--  returned. Only works for finite lists.
--  @any f xs == True@ implies @getLast f xs == (ys,Just e,zs)@
--  such that @xs == ys ++ [e] ++ zs@, @f e == True@ and @any f zs == False@.
--  On the other hand, @any f xs == False@ implies
--  @getLast f xs == ([],Nothing,xs)@.
getLast :: (a -> Bool) -> [a] -> ([a],Maybe a,[a])
getLast f xs = (init' before, lastToMaybe before, after xs)
   where
      after = reverse . takeWhile (not . f) . reverse
      before = take (length xs - length (after xs)) xs

      lastToMaybe [] = Nothing
      lastToMaybe ys = Just $ last ys

      init' [] = []
      init' ys = init ys

-- |Pads a list cs to a length of i with filler elements c such that
--  @padLeft c i cs = cc++cs@ with @cc = [c,...,c]@.
padLeft :: a -> Int -> [a] -> [a]
padLeft c i cs = replicate (i - length cs) c P.++ cs

-- |Pads a list cs to a length of i with filler elements c such that
--  @padLeft c i cs = cs++cc@ with @cc = [c,...,c]@.
padRight :: a -> Int -> [a] -> [a]
padRight c i cs = cs P.++ replicate (i - length cs) c

-- |'padLeft' for 'Text'. This function just unpacks and re-packs
--  the text is is thus not recommended for large inputs.
padLeft' :: Char -> Int -> T.Text -> T.Text
padLeft' c i cs = T.pack $ padLeft c i (T.unpack cs)

-- |'padLeft' for 'Text'. This function just unpacks and re-packs
--  the text is is thus not recommended for large inputs.
padRight' :: Char -> Int -> T.Text -> T.Text
padRight' c i cs = T.pack $ padRight c i (T.unpack cs)

-- |Turns the empty string into Nothing, everything else into Just.
mkNothing :: T.Text -> Maybe T.Text
mkNothing t | T.length t == 0 = Nothing
            | otherwise       = Just t

-- |Concatenates three strings. The third string is put between the
--  first two.
splice :: T.Text -> T.Text -> T.Text -> T.Text
splice b a s = b `T.append` s `T.append` a

-- |Takes @t@ and returns @[t]@ if @length t > 0@. Otherwise,
--  returns @[]@.
toList :: T.Text -> [T.Text]
toList t | T.null t    = []
         | otherwise = [t]

-- |Infix version of 'Text.append'.
(++) :: T.Text -> T.Text -> T.Text
(++) = T.append

-- |Returns True iff the string is composed only of digits and is not empty.
isNum :: String -> Bool
isNum x = all isDigit x && not (null x)

-- |Wrapper around 'show' which turns the output into 'Text'.
--  @show' = pack . show@ and thus, it is quite inefficient. This function
--  should not be used for large texts.
show' :: Show a => a -> T.Text
show' = T.pack . show
