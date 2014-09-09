{-# LANGUAGE OverloadedStrings #-}

-- |Small helper functions relating to strings.
module Helper.String where

import qualified Data.Text as T (Text, append, take, drop, length)

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
appendAbs x y = if T.take 3 y == "../" then x `T.append` stripRel y else y

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
padLeft c i cs = replicate (i - length cs) c ++ cs

-- |Turns the empty string into Nothing, everything else into Just.
mkNothing :: T.Text -> Maybe T.Text
mkNothing t | T.length t == 0 = Nothing
            | otherwise       = Just t

-- |Concatenates three lists. The third list is put between the
--  first two.
splice :: [a] -> [a] -> [a] -> [a]
splice b a s = b ++ s ++ a