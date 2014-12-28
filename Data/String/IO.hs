{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- |Unifies operations on various stringlike types (String, Text, ByteString)
--  under a common class.
--  The rationale for this module is that all the popular string libraries
--  all have the essentially identical interface already, yet conversion between
--  them is a constant hassle.
--
--  For functions that take multiple values (which may be of different type),
--  the 'Data.Types.Injective' is recommended, which provides relatively easy
--  conversion.
module Data.String.IO where

import Prelude hiding (putStr, hPutStr, putStrLn, hPutStrLn, getLine,
                       hGetLine, readFile, writeFile, appendFile, (++),
                       head, tail, null, length, (!!))

import Data.Functor.Monadic
import Data.Types.Injective

import Control.Monad.Catch
import Control.Monad.Trans
import qualified Data.ByteString as BS hiding (hPutStrLn)
import qualified Data.ByteString.Char8 as BS (hPutStrLn)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (Foldable)
import Data.Int (Int64)
import qualified Data.List as LU
import qualified Data.List.Safe as L
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text as TS
import qualified Data.Text.IO as TS
import Data.Traversable (Traversable)
import Data.Word (Word8)
import qualified System.IO as Sy

-- |Types for which common list/string operations are available.
--  If you make a type an instance of this class, make sure to create
--  corresponding type instances for 'Elem' and 'Index'.
--
--  == Safe functions
--  Unsafe functions (head, tail \& co.) have default safe implementations
--  that can resort to appropriate error instead of crashing the program.
--  The Monad can just be a Maybe, but if inspection of the error case is
--  is supported (e.g. Either), it will be one of the following:
--
--  * 'Data.List.Safe.EmptyListException' if the list was empty or too short.
--  * 'Data.List.Safe.NegativeIndexException' if a negative index was accessed.
--
--  == Foldable and Traversable
--  In general, string types are neither (due to them having kind *), but
--  a the configuration @(Foldable t, Traversable t, Stringlike (t e))@ is of
--  course possible for @t :: * -> *@.
class Stringlike s where
   cons :: Elem s -> s -> s
   snoc :: s -> Elem s -> s
   (++) :: s -> s -> s
   null :: s -> Bool
   length :: Integral n => s -> n
   (!!) :: s -> Ind s -> Elem s
   head :: s -> Elem s
   tail :: s -> s
   -- |Safe version of '(!!)'.
   index :: (MonadThrow m, Integral (Ind s)) => s -> Ind s -> m (Elem s)
   index x i | null x        = throwM L.EmptyListException
             | i < 0         = throwM L.NegativeIndexException
             | length x <= i = throwM L.EmptyListException
             | otherwise     = return $ x !! (fromIntegral i)
   -- |Safe version of 'head'.
   head' :: (MonadThrow m) => s -> m (Elem s)
   head' x | null x    = throwM L.EmptyListException
           | otherwise = return $ head x
   -- |Safe version of 'tail'.
   tail' :: (MonadThrow m) => s -> m s
   tail' x | null x    = throwM L.EmptyListException
           | otherwise = return $ tail x

-- |Stringlike types which support IO operations.
class Stringlike s => StringlikeIO s where
   readFile :: (MonadIO m, Injective f String) => f -> m s
   writeFile :: (MonadIO m, Injective f String) => f -> s -> m ()
   appendFile :: (MonadIO m, Injective f String) => f -> s -> m ()
   hPutStr :: (MonadIO m) => Sy.Handle -> s -> m ()
   hPutStrLn :: (MonadIO m) => Sy.Handle -> s -> m ()
   hGetLine :: (MonadIO m) => Sy.Handle -> m s

putStr :: (MonadIO m, StringlikeIO s) => s -> m ()
putStr = liftIO . hPutStr Sy.stdout

putStrLn :: (MonadIO m, StringlikeIO s) => s -> m ()
putStrLn = liftIO . hPutStrLn Sy.stdout

getLine :: (MonadIO m, StringlikeIO s) => m s
getLine = liftIO $ hGetLine Sy.stdin

putErr :: (MonadIO m, StringlikeIO s) => s -> m ()
putErr = liftIO . hPutStr Sy.stderr

putErrLn :: (MonadIO m, StringlikeIO s) => s -> m ()
putErrLn = liftIO . hPutStrLn Sy.stderr


-- Instances
-------------------------------------------------------------------------------

type family Elem s

type instance Elem [a] = a
type instance Elem TS.Text = Char
type instance Elem TL.Text = Char
type instance Elem BS.ByteString = Word8
type instance Elem BL.ByteString = Word8

type family Ind s

type instance Ind [a] = Int
type instance Ind TS.Text = Int
type instance Ind TL.Text = Int64
--type instance Ind BS.ByteString = Word8
--type instance Ind BL.ByteString = Word8

-- String
-------------------------------------------------------------------------------

instance Stringlike [a] where
   cons = (:)
   snoc xs x = xs++[x]
   (++) = (L.++)
   null = L.null
   length = L.genericLength
   (!!) = (LU.!!)
   head = LU.head
   tail = LU.tail
   index = (L.!!)
   head' = L.head
   tail' = L.tail

instance StringlikeIO String where
   readFile = liftIO . Sy.readFile . to
   writeFile f s = liftIO $ Sy.writeFile (to f) s
   appendFile f s = liftIO $ Sy.appendFile (to f) s
   hPutStr f s = liftIO $ Sy.hPutStr (to f) s
   hPutStrLn f s = liftIO $ Sy.hPutStrLn (to f) s
   hGetLine = liftIO . Sy.hGetLine

-- Text
-------------------------------------------------------------------------------

instance Stringlike TS.Text where
   cons = TS.cons
   snoc = TS.snoc
   (++) = TS.append
   null = TS.null
   length = fromIntegral . TS.length
   (!!) = TS.index
   head = TS.head
   tail = TS.tail

instance StringlikeIO TS.Text where
   readFile = liftIO . TS.readFile . to
   writeFile f s = liftIO $ TS.writeFile (to f) s
   appendFile f s = liftIO $ TS.appendFile (to f) s
   hPutStr f s = liftIO $ TS.hPutStr (to f) s
   hPutStrLn f s = liftIO $ TS.hPutStrLn (to f) s
   hGetLine = liftIO . TS.hGetLine


instance Stringlike TL.Text where
   cons = TL.cons
   snoc = TL.snoc
   (++) = TL.append
   null = TL.null
   length = fromIntegral . TL.length
   (!!) = TL.index
   head = TL.head
   tail = TL.tail

instance StringlikeIO TL.Text where
   readFile = liftIO . TL.readFile . to
   writeFile f s = liftIO $ TL.writeFile (to f) s
   appendFile f s = liftIO $ TL.appendFile (to f) s
   hPutStr f s = liftIO $ TL.hPutStr (to f) s
   hPutStrLn f s = liftIO $ TL.hPutStrLn (to f) s
   hGetLine = liftIO . TL.hGetLine


-- ByteString
-------------------------------------------------------------------------------

{-
instance StringlikeIO BS.ByteString Word8 where
   readFile = liftIO . BS.readFile . to
   writeFile f s = liftIO $ BS.writeFile (to f) s
   appendFile f s = liftIO $ BS.appendFile (to f) s
   hPutStr f s = liftIO $ BS.hPut (to f) s
   hPutStrLn f s = liftIO $ BS.hPutStrLn (to f) s
   hGetLine = liftIO . BS.hGetLine

instance StringlikeIO BL.ByteString Word8 where
   readFile = liftIO . BL.readFile . to
   writeFile f s = liftIO $ BL.writeFile (to f) s
   appendFile f s = liftIO $ BL.appendFile (to f) s
   hPutStr f s = liftIO $ BL.hPut (to f) s
   hPutStrLn f s = liftIO $ BS.hPutStrLn (to f) $ BL.toStrict s
   hGetLine = liftIO . (BS.hGetLine >=$> BL.fromStrict)
-}
