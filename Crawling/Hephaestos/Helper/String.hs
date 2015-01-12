{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- |Small helper functions relating to strings.
module Crawling.Hephaestos.Helper.String where

import Prelude
import qualified Prelude as Pr

import Control.Arrow
import Data.Char (isDigit, isAlpha)
import qualified Data.List.Safe as LS
import Data.List.Split
import qualified Data.Text.Lazy as T
import Data.Types.Isomorphic
import qualified Network.URI as N

-- |Strips everything from a text after and including the first @?@
--  character.
stripParams :: T.Text -> T.Text
stripParams = T.takeWhile ('?'/=)

-- |Appends the second URI to the first and normalises the result.
--  Defined as @combineURI x y = show $ (parseURIReference y) `nonStrictRelativeTo` x@.
--  'N.parseURIRefernece' is the most permissive URI parser, accepting both
--  relative and absolute links. If the second argument cannot be
--  parsed as a URI reference, the function returns it unchanged
--  (this is fail-safe behaviour for garbage links).
combineURI :: N.URI -> T.Text -> T.Text
combineURI x y = case N.parseURIReference (T.unpack y) of
                    Just y' -> showT $ y' `N.nonStrictRelativeTo` x
                    Nothing -> y

-- |Synonym of 'N.nonStrictRelativeTo'. This function is equivalent
--  to 'combineURI'.
combineURI' :: N.URI -> N.URI -> N.URI
combineURI' = N.nonStrictRelativeTo

-- |Gets the last element of a list which fulfils a given predicate.
--  The elements of the list before and after that element are also
--  returned. Only works for finite lists.
--  @any f xs == True@ implies @getLast f xs == (ys,Just e,zs)@
--  such that @xs == ys ++ [e] ++ zs@, @f e == True@ and @any f zs == False@.
--  On the other hand, @any f xs == False@ implies
--  @getLast f xs == ([],Nothing,xs)@.
getLast :: (a -> Bool) -> [a] -> ([a],Maybe a,[a])
getLast f xs = (concat $ LS.init before, LS.last before, after xs)
   where
      after = reverse . takeWhile (not . f) . reverse
      before = take (length xs - length (after xs)) xs

-- |Pads a list cs to a length of i with filler elements c such that
--  @padLeft c i cs = cc++cs@ with @cc = [c,...,c]@.
padLeft :: a -> Int -> [a] -> [a]
padLeft c i cs = replicate (i - length cs) c Pr.++ cs

-- |Pads a list cs to a length of i with filler elements c such that
--  @padLeft c i cs = cs++cc@ with @cc = [c,...,c]@.
padRight :: a -> Int -> [a] -> [a]
padRight c i cs = cs Pr.++ replicate (i - length cs) c

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

-- |Returns True iff the string is composed only of digits and is not empty.
isNum :: String -> Bool
isNum x = all isDigit x && not (null x)

-- |Wrapper around 'show' which turns the output into 'Text'.
--  @show' = pack . show@ and thus, it is quite inefficient. This function
--  should not be used for large texts.
showT :: Show a => a -> T.Text
showT = T.pack . show

-- |Case-insensitive and whitespace-removing 'elem'.
elem' :: T.Text -> [T.Text] -> Bool
elem' t ts = clean t `elem` map clean ts
   where clean = T.strip . T.toLower
