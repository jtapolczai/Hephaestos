{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}

-- |Functions to expedite the building of REPLs.
module System.REPL (
   -- *Text-versions of Prelude Functions
   putStrLnT,
   putStrT,
   putErrLnT,
   hPutStrLnT,
   hPutStrT,
   getLineT,
   -- **Lifted versions
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
   -- *Command dispatch
   -- |Using the 'Command' class is not necessary, but it makes dealing with
   --  user input considerably easier. When a command is run with a line of
   --  input, it automatically segments it by whitespace, tries to interpret
   --  each part as one of its arguments and passes them to the actual command
   --  function. If any arguments haven't been supplies, it asks for them on
   --  stdin. If too many arguments have been supplied, or if any argument'
   --  parsing returns an error, the command is aborted.
   --
   --  Example:
   --
   --  > cd = makeCommand1 ...
   --
   --  >>> :cd ../
   --  Directory changed!
   --  >>> :cd
   --  Enter new directory:
   --  >>> ../
   --  Directory changed!
   Command(..),
   commandInfo,
   runOnce,
   commandDispatch,
   summarizeCommands,
   -- ** Making commands.
   makeCommand,
   makeCommand1,
   makeCommand2,
   makeCommand3,
   makeCommand4,
   ) where

import Prelude hiding (putStrLn, putStr, getLine, unwords, words, (!!), (++))
import qualified Prelude as P

import Control.Arrow (right, (+++), left)
import Control.Monad
import Control.Monad.State
import Data.Char (isSpace)
import Data.Either (lefts)
import Data.Either.Optional
import Data.Functor ((<$>))
import qualified Data.List as L
import Data.Maybe (listToMaybe, fromJust, isNothing)
import Data.Ord
import Data.Text
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import Crawling.Hephaestos.Helper.Functor
import Crawling.Hephaestos.Helper.String ((++), padRight', show')
import System.IO hiding (putStrLn, putStr, getLine)
import Text.Read (readMaybe)

import System.REPL.Command.Helper

-- Stdio
--------------------------------------------------------------------------------

-- |'Text'-analogue of 'putStrLn'.
putStrLnT :: Text -> IO ()
putStrLnT = P.putStrLn . unpack

-- |'Text'-analogue of 'putStr'.
putStrT :: Text -> IO ()
putStrT = P.putStr . unpack

-- |'Text'-analogue of 'hPutStrLn stderr'
putErrLnT :: Text -> IO ()
putErrLnT = System.IO.hPutStrLn stderr . unpack

-- |'Text'-analogue of 'getLine'.
getLineT :: IO Text
getLineT = liftM pack P.getLine

-- |'Text'-analogue of 'hPutStrLn'.
hPutStrLnT :: Handle -> Text -> IO ()
hPutStrLnT h = System.IO.hPutStrLn h . unpack

-- |'Text'-analogue of 'hPutStrLn'.
hPutStrT :: Handle -> Text -> IO ()
hPutStrT h = System.IO.hPutStr h . unpack

-- |Prints @> @ and asks the user to input a line.
prompt :: MonadIO m => m Text
prompt = prompt' "> "

-- |Prints its first argument and, in the same line, asks the user
--  to input a line.
prompt' :: MonadIO m => Text -> m Text
prompt' s = putStr s >> liftIO (hFlush stdout) >> getLine

-- |Lifted version of 'putStrLnT'.
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . putStrLnT

-- |Lifted version of 'putStrT'.
putStr :: MonadIO m => Text -> m ()
putStr = liftIO . putStrT

-- |Lifted version of 'getLineT'.
getLine :: MonadIO m => m Text
getLine = liftIO getLineT

-- |Lifted version of 'putErrLnT'
putErrLn :: MonadIO m => Text -> m ()
putErrLn = liftIO . putErrLnT


-- Askers
--------------------------------------------------------------------------------

-- |The description of an \'ask for user input\'-action.
--  The type parameters are the used monad (typically @IO@),
--  the type of the read value and the type of the error that is thrown
--  in case of failures.
data Asker m a e = Asker{ -- |The prompt to be displayed to the user.
                          askerPrompt::Text,
                          -- |The parser for the input value, which
                          --  either delivers a value of type @a@ or
                          --  an error message.
                          askerParser::Text -> Either e a,
                          -- |The predicate which the input, once read,
                          --  must fulfill. It either delivers 'Success'
                          --  or an error message.
                          askerPredicate::a -> m (Either e Success)}

-- |Singleton type representing success.
data Success = Success deriving (Eq, Show, Read)

-- |Represents a failure of an ask function.
--  It can either be a type failure (failure to interpret the
--  user input as a value of the required type) or a predicate
--  failure (the user input could be interpreter as a value
--  of the required type, but it failed some user-supplied test).
data AskFailure e = TypeFailure e -- ^Indicates that the parsing as the
                                  --  required type failed.
                  | PredicateFailure e -- ^Indiciates that the parsed
                                       -- value failed a predicate.
                  | ParamNumFailure -- ^Indicates that an incorrect number of
                                    --  parameters was passed.
                  | NothingFoundFailure -- ^Indicates that no action was
                                        --  appropriate to the given input.

-- |Gets the failure text from an 'AskFailure'.
failureText :: AskFailure e -> e
failureText (TypeFailure t) = t
failureText (PredicateFailure t) = t

-- |A verbatim Text whose Read instance simply returns the read
--  string, as-is.
--  This is useful for askers which ask for strings without quotes.
newtype Verbatim = Verbatim{fromVerbatim::Text -- ^Extracts a 'Verbatim''s 'Text'.
                            }

-- |Read-instance for 'Verbatim'. Wraps the given value into quotes and
--  reads it a a 'Text'.
instance Read Verbatim where
   readsPrec _ s = [(Verbatim $ pack s,"")]

-- |Creates a general 'Asker' with 'Data.Read.readMaybe' as its parser.
--  This suffices for most simple values.
--  The main drawback of using 'Data.Read.readMaybe' is that the input
--  'Text' is unpacked into a String, which incurs a performance hit.
--  For short (one-line) input. this isn't important, but if large ones
--  are expected, it's better to pass a custom, 'Text'-compatible parsing
--  function, such as a parsec-parser.
asker :: (Monad m, Functor m, Read a)
      => Text -- ^The prompt.
      -> e -- ^Type error message.
      -> e -- ^Predicate error message.
      -> (a -> m Bool) -- ^Predicate.
      -> Asker m a e
asker pr errT errP pred = Asker pr parse check
   where
      parse = toEither errT . readMaybe . T.unpack
      check = pred >=$> (\case True  -> Right Success
                               False -> Left errP)

-- |Creates an 'Asker' which just cares about the type of the input.
typeAsker :: (Monad m, Functor m, Read a)
          => Text -- ^The prompt.
          -> e -- ^Type error message.
          -> Asker m a e
typeAsker p errT = asker p errT undefined (const $ return True)

-- |Creates an 'Asker' which takes its input verbatim as 'Text'.
predAsker :: (Monad m, Functor m)
          => Text -- ^The prompt.
          -> e -- ^Predicate error message.
          -> (Text -> m Bool) -- ^The predicate.
          -> Asker m Verbatim e
predAsker p errP f = asker p undefined errP (f . fromVerbatim)

-- |An asker which asks for an optional value. If only whitespace
--  is entered (according to 'Data.Char.isSpace'), it returns 'Nothing'
--  without further parsing or checking; otherwise, it behaves identically
--  to 'asker'.
maybeAsker :: (Monad m, Functor m, Read a)
           => Text -- ^The prompt.
           -> e -- ^Type error message.
           -> e -- ^Predicate error message.
           -> (a -> m Bool) -- ^Predicate.
           -> Asker m (Maybe a) e
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
ask :: (MonadIO m, Functor m, Read a)
    => Asker m a e
    -> Maybe Text
    -> m (Either (AskFailure e) a)
ask a v = case v of Nothing -> (liftIO . prompt' . askerPrompt $ a) >>= check
                    Just x -> check x
   where
      check inp =
         case askerParser a inp of
            Left err -> return $ Left $ TypeFailure err
            Right t -> askerPredicate a t >$> (PredicateFailure +++ const t)

-- |See 'ask'. Always reads the input from stdin.
--  @ask' a = ask a Nothing@.
ask' :: (MonadIO m, Functor m, Read a)
     => Asker m a e -> m (Either (AskFailure e) a)
ask' a = ask a Nothing

-- |Repeatedly executes an ask action until the user enters a valid value.
--  Error messages are printed each time.
untilValid :: (MonadIO m, Functor m, Read a, Show e)
           => m (Either (AskFailure e) a) -> m a
untilValid m = do m' <- m
                  case m' of (Left l) -> putStrLn (show' $ failureText l)
                                         >> untilValid m
                             (Right r) -> return r



-- Commands
--------------------------------------------------------------------------------

-- |A REPL command, possibly with parameters.
data Command m a = Command{
                  -- |The short name of the command. Purely informative.
                  commandName :: Text,
                  -- |Returns whether a string matches
                  --  a command name. The simplest form is
                  --  @s==@ for some string s, but more liberal
                  --  matchings are possible.
                  commandTest :: Text -> Bool,
                  -- |A description of the command.
                  commandDesc :: Text,
                  -- |The number of parameters, if fixed.
                  numParameters :: Maybe Int,
                  -- |Runs the command with the input text as parameter.
                  runCommand :: Text -> m a}

-- |Prints information (the command name, description and, if given,
--  the number of parameters) about a command to the console.
commandInfo :: MonadIO m => Command m a -> m ()
commandInfo c = do putStr $ commandName c
                   putStrLn $ maybe "" (pack . (" Parameters: " P.++) . show) (numParameters c)
                   putStrLn $ commandDesc c

-- |Splits and cleans the input of a command.
--  Be aware that this is not a security-related function!
--  Defined as @map strip . words . strip@.
sanitize :: Text -> [Text]
sanitize = Prelude.map strip . words . strip

-- |Takes a line of text and a command.
--  If the text matches the given command's 'commandTest',
--  the command is run with it. If not, 'Nothing' is returned.
runOnce :: MonadIO m => Text -> Command m a -> m (Maybe a)
runOnce l c = if commandTest c l then liftM Just (runCommand c l)
                                 else return Nothing

-- |Prints an error message if an unexpected number of parameters have been
--  supplied.
paramErr :: MonadIO m
         => Text -- ^The command name.
         -> Text -- ^The given input.
         -> Int  -- ^The expected number of parameters.
         -> m ()
paramErr c inp 0 = putErrLn $ "Parameters " ++ unwords (tail' $ sanitize inp)
                              ++ " given to " ++ c ++ ". "
                              ++ c ++ " takes no parameters."
   where tail' [] = []
         tail' (x:xs) = xs
paramErr c inp n = putErrLn $ l ++ " parameters given to " ++ c
                              ++ ". " ++ c ++ " takes "
                              ++ n' ++ " parameters."
   where l = pack . show . (\x -> x-1) . P.length . sanitize $ inp
         n' = pack.show $ n


-- |Checks the number of parameters before executing a monadic function.
checkParams :: MonadIO m
            => Text -- ^The command name.
            -> Text -- ^The raw input (including the command name).
            -> Int -- ^The expected number of parameters, excluding the command's name itself.
            -> ([Text] -> m (Either (AskFailure e) a)) -- ^The command.
            -> m (Either (AskFailure e) a) -- ^Result. If too many parameters were
                                           --  passed, this will be a 'ParamNumFailure'.
checkParams n inp num m =
   if P.length (sanitize inp) > (num + 1) then paramErr n inp num
                                               >> return (Left ParamNumFailure)
   else m (sanitize inp)

-- |Throws a user error if a command was called on an empty command string
--  (which is illegal, as the command string at least has to contain the command's
--  non-empty name).
noStringErr :: a
noStringErr = error "Called command with empty command string. Expected at least the command name."

-- |Creates a command without parameters.
makeCommand :: (MonadIO m)
            => Text -- ^Command name.
            -> (Text -> Bool) -- ^Command test.
            -> Text -- ^Command description.
            -> (Text -> m a) -- ^The actual command.
            -> Command m (Either (AskFailure e) a)
makeCommand n t d f = Command n t d (Just 0) (\inp -> checkParams n inp 0 c)
   where
      c inp = liftEM f (toEither noStringErr (listToMaybe inp))

-- |Creates a command with one parameter.
makeCommand1 :: (MonadIO m, Functor m, Read a, Show e)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a e -- ^'Asker' for the first parameter.
             -> (Text -> a -> m z)
             -> Command m (Either (AskFailure e) z)
makeCommand1 n t d p1 f = Command n t d (Just 1) (\inp -> checkParams n inp 1 c)
   where
      c inp = do let li = toEither noStringErr (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 printError [voidR x1]
                 liftEM2 f li x1

-- |Creates a command with two parameters.
makeCommand2 :: (MonadIO m, Functor m, Show e, Read a, Read b)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a e -- ^'Asker' for the first parameter.
             -> Asker m b e -- ^'Asker' for the second perameter.
             -> (Text -> a -> b -> m z)
             -> Command m (Either (AskFailure e) z)
makeCommand2 n t d p1 p2 f = Command n t d (Just 2) (\inp -> checkParams n inp 2 c)
   where
      c inp = do let li = toEither noStringErr (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 x2 <- onSucc x1 $ ask p2 (inp !! 2)
                 printError [voidR x1, voidR x2]
                 liftEM3 f li x1 x2

-- |Creates a command with three parameters.
makeCommand3 :: (MonadIO m, Functor m, Show e, Read a, Read b, Read c)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a e -- ^'Asker' for the first parameter.
             -> Asker m b e -- ^'Asker' for the second perameter.
             -> Asker m c e -- ^'Asker' for the third parameter.
             -> (Text -> a -> b -> c -> m z)
             -> Command m (Either (AskFailure e) z)
makeCommand3 n t d p1 p2 p3 f = Command n t d (Just 3) (\inp -> checkParams n inp 3 c)
   where
      c inp = do let li = toEither noStringErr (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 x2 <- onSucc x1 $ ask p2 (inp !! 2)
                 x3 <- onSucc x2 $ ask p3 (inp !! 3)
                 printError [voidR x1, voidR x2, voidR x3]
                 liftEM4 f li x1 x2 x3

-- |Creates a command with four parameters.
makeCommand4 :: (MonadIO m, Functor m, Show e, Read a, Read b, Read c, Read d)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a e -- ^'Asker' for the first parameter.
             -> Asker m b e -- ^'Asker' for the second perameter.
             -> Asker m c e -- ^'Asker' for the third parameter.
             -> Asker m d e -- ^'Asker' for the fourth parameter.
             -> (Text -> a -> b -> c -> d -> m z)
             -> Command m (Either (AskFailure e) z)
makeCommand4 n t d p1 p2 p3 p4 f = Command n t d (Just 4) (\inp -> checkParams n inp 4 c)
   where
      c inp = do let li = toEither noStringErr (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 x2 <- onSucc x1 $ ask p2 (inp !! 2)
                 x3 <- onSucc x2 $ ask p3 (inp !! 3)
                 x4 <- onSucc x3 $ ask p4 (inp !! 4)
                 printError [voidR x1, voidR x2, voidR x3, void x4]
                 liftEM5 f li x1 x2 x3 x4


-- |Prints the first AskFailure in a list of values, if there is one.
printError :: (MonadIO m, Show e) => [Either (AskFailure e) b] -> m ()
printError xs = case lefts xs of []    -> return ()
                                 (x:_) -> putErrLn $ T.pack $ show $ failureText x


-- |Takes an input and tries to run it against a list of commands,
--  trying the out in sequence. The first command whose 'commandTest'
--  returns True is executed. If none of the commands match,
--  @m (Left NothingFoundFailure)@ is returned.
commandDispatch :: (Monad m, Functor m)
                => Text -- ^The user's input.
                -> [Command m (Either (AskFailure e) z)] -- ^The command library.
                -> m (Either (AskFailure e) z)
commandDispatch input cs =
   if P.null input' || noMatch then return $ Left NothingFoundFailure
   else runCommand (fromJust first) input
   where
      input' = sanitize input
      noMatch = isNothing first
      first = listToMaybe $ P.dropWhile (not . flip commandTest (P.head input')) cs


-- |Prints out a list of command names, with their descriptions.
summarizeCommands :: MonadIO m
                  => [Command m2 (Either a b)]
                  -> m ()
summarizeCommands [] = return ()
summarizeCommands xs@(_:_) = mapM_ (\c -> prName c >> prDesc c) xs
   where
      maxLen :: Int
      maxLen = T.length
               $ commandName
               $ L.minimumBy (comparing $ (* (-1)) . T.length . commandName) xs
      prName = putStr . padRight' ' ' maxLen . commandName
      prDesc = putStrLn . (" - " ++) . commandDesc
