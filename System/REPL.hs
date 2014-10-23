{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

-- |Functions to expedite the building of REPLs.
module System.REPL (
   -- *String- and monad-generic versions of Prelude Functions
   putStrLn,
   putStr,
   putErrLn,
   hPutStrLn,
   hPutStr,
   getLine,
   -- *Feture-rich reading of user-input
   -- |These functions automate parsing and validating command-line
   --  input via the 'Asker' type.
   Asker(..),
   Success(..),
   AskFailure(..),
   asker,
   typeAsker,
   predAsker,
   maybeAsker,
   prompt,
   prompt',
   Verbatim(..),
   -- **Asking for input
   ask,
   ask',
   untilValid,
   ) where

import Prelude hiding (putStrLn, putStr, getLine, unwords, words, (!!), (++))
import qualified Prelude as P

import Control.Arrow (right, (|||), (+++), left)
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Char (isSpace)
import Data.Either (lefts)
import Data.Either.Optional
import Data.Functor.Monadic
import qualified Data.List as L
import Data.Maybe (listToMaybe, fromJust, isNothing)
import Data.Ord
import Data.Text (Text)
import Data.Typeable
import qualified Data.Text as T
import Data.Types.Isomorphic
import Data.Text.Lazy.Builder (toLazyText)
import Crawling.Hephaestos.Helper.String ((++), padRight', showT)
import qualified System.IO as IO
import Text.Read (readMaybe)

-- Stdio
--------------------------------------------------------------------------------

-- |'Text'-analogue of 'putStrLn'.
putStrLn :: (MonadIO m, Injective a String) => a -> m ()
putStrLn = liftIO . P.putStrLn . to

-- |'Text'-analogue of 'putStr'.
putStr :: (MonadIO m, Injective a String) => a -> m ()
putStr = liftIO . P.putStr . to

-- |'Text'-analogue of 'hPutStrLn stderr'
putErrLn :: (MonadIO m, Injective a String) => a -> m ()
putErrLn = liftIO . IO.hPutStrLn IO.stderr . to

-- |'Text'-analogue of 'getLine'.
getLine :: (MonadIO m, Functor m, Injective String a) => m a
getLine = liftIO P.getLine >$> to

-- |'Text'-analogue of 'hPutStrLn'.
hPutStrLn :: (MonadIO m, Injective a String) => IO.Handle -> a -> m ()
hPutStrLn h = liftIO . IO.hPutStrLn h . to

-- |'Text'-analogue of 'hPutStrLn'.
hPutStr :: (MonadIO m, Injective a String) => IO.Handle -> a -> m ()
hPutStr h = liftIO . IO.hPutStr h . to

-- |Prints @> @ and asks the user to input a line.
prompt :: (MonadIO m, Functor m, Injective String a) => m a
prompt = prompt' ("> " :: String)

-- |Prints its first argument and, in the same line, asks the user
--  to input a line.
prompt' :: (MonadIO m, Functor m, Injective a String, Injective String b)
        => a -> m b
prompt' s = putStr s >> liftIO (IO.hFlush IO.stdout) >> getLine

-- Askers
--------------------------------------------------------------------------------

-- |The description of an \'ask for user input\'-action.
--  The type parameters are the used monad (typically @IO@),
--  the type of the read value and the type of the error that is thrown
--  in case of failures.
data Asker m a = Asker{ -- |The prompt to be displayed to the user.
                        askerPrompt::Text,
                        -- |The parser for the input value, which
                        --  either delivers a value of type @a@ or
                        --  an error message.
                        askerParser::Text -> Either Text a,
                        -- |The predicate which the input, once read,
                        --  must fulfill. It either delivers 'Success'
                        --  or an error message.
                        askerPredicate::a -> m (Either Text Success)}

-- |Singleton type representing success.
data Success = Success deriving (Eq, Show, Read)

-- |Represents a failure of an ask function.
--  It can either be a type failure (failure to interpret the
--  user input as a value of the required type) or a predicate
--  failure (the user input could be interpreter as a value
--  of the required type, but it failed some user-supplied test).
data AskFailure = TypeFailure Text -- ^Indicates that the parsing as the
                                   --  required type failed.
                  | PredicateFailure Text -- ^Indiciates that the parsed
                                          -- value failed a predicate.
                  | ParamFailure Text
                    -- ^Indicates that an incorrect number of
                    --  parameters was passed.
                  | NothingFoundFailure -- ^Indicates that no action was
                                        --  appropriate to the given input.
   deriving (Typeable, Eq)

instance Exception AskFailure

instance Show AskFailure where
   show (ParamFailure t) = T.unpack t
   show NothingFoundFailure = "No appropriate action found!"
   show (PredicateFailure t) = T.unpack t
   show (TypeFailure t) = T.unpack t

-- |A verbatim Text whose Read instance simply returns the read
--  string, as-is.
--  This is useful for askers which ask for strings without quotes.
newtype Verbatim = Verbatim{fromVerbatim::Text -- ^Extracts a 'Verbatim''s 'Text'.
                            }

-- |Read-instance for 'Verbatim'. Wraps the given value into quotes and
--  reads it a a 'Text'.
instance Read Verbatim where
   readsPrec _ s = [(Verbatim $ T.pack s,"")]

-- |Creates a general 'Asker' with 'Data.Read.readMaybe' as its parser.
--  This suffices for most simple values.
--  The main drawback of using 'Data.Read.readMaybe' is that the input
--  'Text' is unpacked into a String, which incurs a performance hit.
--  For short (one-line) input. this isn't important, but if large ones
--  are expected, it's better to pass a custom, 'Text'-compatible parsing
--  function, such as a parsec-parser.
asker :: (Monad m, Functor m, Read a)
      => Text -- ^The prompt.
      -> Text -- ^Type error message.
      -> Text -- ^Predicate error message.
      -> (a -> m Bool) -- ^Predicate.
      -> Asker m a
asker pr errT errP pred = Asker pr parse check
   where
      parse = toEither errT . readMaybe . T.unpack
      check = pred >=$> (\case True  -> Right Success
                               False -> Left errP)

-- |Creates an 'Asker' which just cares about the type of the input.
typeAsker :: (Monad m, Functor m, Read a)
          => Text -- ^The prompt.
          -> Text -- ^Type error message.
          -> Asker m a
typeAsker p errT = asker p errT undefined (const $ return True)

-- |Creates an 'Asker' which takes its input verbatim as 'Text'.
predAsker :: (Monad m, Functor m)
          => Text -- ^The prompt.
          -> Text -- ^Predicate error message.
          -> (Text -> m Bool) -- ^The predicate.
          -> Asker m Verbatim
predAsker p errP f = asker p undefined errP (f . fromVerbatim)

-- |An asker which asks for an optional value. If only whitespace
--  is entered (according to 'Data.Char.isSpace'), it returns 'Nothing'
--  without further parsing or checking; otherwise, it behaves identically
--  to 'asker'.
maybeAsker :: (Monad m, Functor m, Read a)
           => Text -- ^The prompt.
           -> Text -- ^Type error message.
           -> Text -- ^Predicate error message.
           -> (a -> m Bool) -- ^Predicate.
           -> Asker m (Maybe a)
maybeAsker pr errT errP pred = Asker pr parse check
   where
      parse t = if T.all isSpace t then right Just
                                        $ toEither errT
                                        $ readMaybe
                                        $ T.unpack t
                                   else Right Nothing

      check Nothing = return $ Right Success
      check (Just t) = pred t >$> (\case True  -> Right Success
                                         False -> Left errP)

-- Running askers
--------------------------------------------------------------------------------

-- |Executes an 'Asker'. If the Text argument is Nothing, the user is asked
--  to enter a line on stdin. If it is @Just x@, @x@ is taken to be input.
--  If the input is of the wrong type, an error-message is printed
--  and the user is asked again.
--  In addition to the condition that the input must be of the correct
--  type, it must also fulfill a predicate.
--
--  Since the predicate is of monadic, arbitrarily complex
--  tests can be performed: checking whether an item is in a database,
--  whether a date was less than x years ago, etc.
ask :: (MonadIO m, MonadError SomeException m, Functor m, Read a)
    => Asker m a
    -> Maybe Text
    -> m a
ask a v = case v of Nothing -> (liftIO . prompt' . askerPrompt $ a) >>= check
                    Just x -> check x
   where
      check inp =
         case askerParser a inp of
            Left err -> throwError $ SomeException $ TypeFailure err
            Right t -> askerPredicate a t
                       >>= (throwError . SomeException . PredicateFailure ||| return . const t)

-- |See 'ask'. Always reads the input from stdin.
--  @ask' a = ask a Nothing@.
ask' :: (MonadIO m, MonadError SomeException m, Functor m, Read a)
     => Asker m a
     -> m a
ask' a = ask a Nothing

-- |Repeatedly executes an ask action until the user enters a valid value.
--  Error messages are printed each time.
untilValid :: (MonadIO m, MonadError SomeException m, Functor m, Read a)
           => m a
           -> m a
untilValid m = m `catchError` (\l -> putStrLn (showT l) >> untilValid m)
