{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}

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
   Asker(..),
   AskFailure(..),
   asker,
   typeAsker,
   predAsker,
   prompt,
   prompt',
   Verbatim(..),
   -- **Asking for input
   ask,
   ask',
   untilValid,
   -- *Command dispatch
   Command(..),
   commandInfo,
   runOnce,
   makeCommand,
   makeCommand1,
   makeCommand2,
   makeCommand3,
   makeCommand4,
   ) where

import Prelude hiding (putStrLn, putStr, getLine, unwords, words, (!!), (++))
import qualified Prelude as P

import Control.Monad
import Control.Monad.State
import Data.Either.Optional
import Data.Maybe (listToMaybe)
import Data.Text
import Helper.String ((++))
import System.IO hiding (putStrLn, putStr, getLine)
import Text.Read (readMaybe)

-- |'Text'-analogue of 'putStrLn'.
putStrLnT :: Text -> IO ()
putStrLnT = P.putStrLn . unpack

-- |'Text'-analogue of 'putStr'.
putStrT :: Text -> IO ()
putStrT = P.putStr . unpack

-- |'Text'-analogue of 'hPutStrLn stderr'
putErrLnT :: Text -> IO ()
putErrLnT = System.IO.hPutStr stderr . unpack

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
prompt :: IO Text
prompt = prompt' "> "

-- |Prints its first argument and, in the same line, asks the user
--  to input a line.
prompt' :: Text -> IO Text
prompt' s = putStrT s >> hFlush stdout >> getLineT

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

-- |The description of an \'ask for user input\'-action.
data Asker m a = Asker{
                        -- ^The prompt to be displayed to the user.
                        askerPrompt::Text,
                        -- ^The error message if the input can't
                        --  be interpreted as a value of type @a@.
                        askerTypeError::Text,
                        -- ^The error message if the input can
                        --  be read as a value of the correct type,
                        --  but fails the predicate.
                        askerValueError::Text,
                        -- ^The predicate which the input, once read,
                        --  must fulfill.
                        askerPredicate::a -> m Bool}


-- |Represents a failure of a ask function.
--  It can either be a type failure (failure to interpret the
--  user input as a value of the required type) or a predicate
--  failure (the user input could be interpreter as a value
--  of the required type, but it failed some user-supplied test).
data AskFailure = TypeFailure Text | PredicateFailure Text

-- |Gets the failure text from an 'AskFailure'.
failureText :: AskFailure -> Text
failureText (TypeFailure t) = t
failureText (PredicateFailure t) = t

-- |A verbatim Text whose Read instance simply returns the read
--  string, as-is.
--  This is useful for askers which ask for strings without quotes.
newtype Verbatim = Verbatim{fromVerbatim::Text}

instance Read Verbatim where
   readsPrec _ s = [(Verbatim $ pack s,"")]


-- |Creates a general 'Asker'.
asker :: (MonadIO m, Read a)
      => Text -- ^The prompt.
      -> Text -- ^Type error message.
      -> Text -- ^Predicate error message.
      -> (a -> m Bool) -- ^Predicate.
      -> Asker m a
asker = Asker

-- |Creates an 'Asker' which just cares about the type of the input.
typeAsker :: (MonadIO m, Read a)
          => Text -- ^The prompt.
          -> Text -- ^Type error message.
          -> Asker m a
typeAsker p eT = Asker p eT undefined (const $ return True)

-- |Creates an 'Asker' which takes its input verbatim as 'Text'.
predAsker :: MonadIO m
          => Text -- ^The prompt.
          -> Text -- ^Predicate error message.
          -> (Text -> m Bool) -- ^The predicate.
          -> Asker m Verbatim
predAsker p eP f = Asker p undefined eP (f . fromVerbatim)

-- |Executes an 'Asker'. If the Text argument is Nothing, the user is asked
--  to enter a line on stdio. If it is @Just x@, @x@ is taken to be input.
--  If the input is of the wrong type, an error-message is printed
--  and the user is asked again.
--  In addition to the condition that the input must be of the correct
--  type, it must also fulfill a predicate.
--
--  Since the predicate is of type @a -> m Bool@, arbitrarily complex
--  tests can be performed: checking whether an item is in a database,
--  whether a date was less than x years ago, etc.
ask :: (MonadIO m, Read a)
    => Asker m a
    -> Maybe Text
    -> m (Either AskFailure a)
ask a v = case v of Nothing -> (liftIO . prompt' . askerPrompt $ a) >>= check
                    Just x -> check x
   where
      check inp = case readMaybe $ unpack inp of
                     Nothing -> return $ Left $ TypeFailure $ askerTypeError a
                     Just t' -> do ok <- askerPredicate a t'
                                   return (if ok then Right t'
                                           else Left
                                                $ PredicateFailure
                                                $ askerValueError a)

-- |See 'ask'. Always reads the input from stdio.
--  @ask' a = ask a Nothing@.
ask' :: (MonadIO m, Read a) => Asker m a -> m (Either AskFailure a)
ask' a = ask a Nothing

-- |Repeatedly executes an ask action until the user enters a valid value.
untilValid :: (MonadIO m, Read a) => m (Either AskFailure a) -> m a
untilValid m = do m' <- m
                  case m' of (Left l) -> putStrLn (failureText l) >> untilValid m
                             (Right r) -> return r

--  |A REPL command, possibly with parameters.
data Command m a = Command{
                  -- ^The short name of the command. Purely informative.
                  commandName :: Text,
                  -- ^Returns whether a string matches
                  -- a command name. The simplest form is
                  -- @s==@ for some string s, but more liberal
                  -- matchings are possible.
                  commandTest :: Text -> Bool,
                  -- ^A description of the command.
                  commandDesc :: Text,
                  -- ^The number of parameters, if fixed.
                  numParameters :: Maybe Int,
                  -- ^Runs the command with the input text as parameter.
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
paramErr c inp 0 = putErrLn $ "Parameters '" ++ unwords (sanitize inp)
                              ++ "' given to '" ++ c ++ "'. "
                              ++ c ++ " takes no parameters."
paramErr c inp n = putErrLn $ l ++ " parameters given to '" ++ c
                              ++ "'. " ++ c ++ " takes "
                              ++ n' ++ " parameters."
   where l = pack . show . (\x -> x-1) . P.length . sanitize $ inp
         n' = pack.show $ n

liftEM :: Monad m => (a -> m z) -> Either x1 a -> m (Either () z)
liftEM _ (Left _) = return $ Left ()
liftEM f (Right r1) = liftM Right $ f r1

liftEM2 :: Monad m
        => (a -> b -> m z)
        -> Either x a
        -> Either x b
        -> m (Either () z)
liftEM2 f v1 v2
   | allHasValue [Opt v1, Opt v2] =
      liftM Right $ getValue (liftM2 f v1 v2)
   | otherwise = return $ Left ()

liftEM3 :: Monad m
        => (a -> b -> c -> m z)
        -> Either x a
        -> Either x b
        -> Either x c
        -> m (Either () z)
liftEM3 f v1 v2 v3
   | allHasValue [Opt v1, Opt v2, Opt v3] =
      liftM Right $ getValue (liftM3 f v1 v2 v3)
   | otherwise = return $ Left ()

liftEM4 :: Monad m
        => (a -> b -> c -> d -> m z)
        -> Either x a
        -> Either x b
        -> Either x c
        -> Either x d
        -> m (Either () z)
liftEM4 f v1 v2 v3 v4
   | allHasValue [Opt v1, Opt v2, Opt v3, Opt v4] =
      liftM Right $ getValue (liftM4 f v1 v2 v3 v4)
   | otherwise = return $ Left ()

liftEM5 :: Monad m
        => (a -> b -> c -> d -> e -> m z)
        -> Either x a
        -> Either x b
        -> Either x c
        -> Either x d
        -> Either x e
        -> m (Either () z)
liftEM5 f v1 v2 v3 v4 v5
   | allHasValue [Opt v1, Opt v2, Opt v3, Opt v4, Opt v5] =
      liftM Right $ getValue (liftM5 f v1 v2 v3 v4 v5)
   | otherwise = return $ Left ()

checkParams :: MonadIO m
            => Text
            -> Text
            -> Int
            -> ([Text] -> m (Either () a))
            -> m (Either () a)
checkParams n inp num m =
   if P.length (sanitize inp) > (num + 1) then paramErr n inp num >> return (Left ())
   else m (sanitize inp)

-- |Creates a command without parameters.
makeCommand :: MonadIO m
            => Text -- ^Command name.
            -> (Text -> Bool) -- ^Command test.
            -> Text -- ^Command description.
            -> (Text -> m a) -- ^The actual command.
            -> Command m (Either () a)
makeCommand n t d f = Command n t d (Just 0) (\inp -> checkParams n inp 1 c)
   where
      c inp = liftEM f (m2e "" (listToMaybe inp))

-- |Creates a command with one parameter.
makeCommand1 :: (MonadIO m, Read a)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> (Text -> a -> m z)
             -> Command m (Either () z)
makeCommand1 n t d p1 f = Command n t d (Just 1) (\inp -> checkParams n inp 1 c)
   where
      c inp = do let li = m2e "" (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 liftEM2 f li x1

-- |Creates a command with two parameters.
makeCommand2 :: (MonadIO m, Read a, Read b)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second perameter.
             -> (Text -> a -> b -> m z)
             -> Command m (Either () z)
makeCommand2 n t d p1 p2 f = Command n t d (Just 2) (\inp -> checkParams n inp 2 c)
   where
      c inp = do let li = m2e "" (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 x2 <- onSucc x1 $ ask p2 (inp !! 2)
                 liftEM4 f li x1 x2

-- |Creates a command with three parameters.
makeCommand3 :: (MonadIO m, Read a, Read b, Read c)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second perameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> (Text -> a -> b -> c -> m z)
             -> Command m (Either () z)
makeCommand3 n t d p1 p2 p3 f = Command n t d (Just 3) (\inp -> checkParams n inp 3 c)
   where
      c inp = do let li = m2e "" (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 x2 <- onSucc x1 $ ask p2 (inp !! 2)
                 x3 <- onSucc x2 $ ask p3 (inp !! 3)
                 liftEM4 f li x1 x2 x3

-- |Creates a command with four parameters.
makeCommand4 :: (MonadIO m, Read a, Read b, Read c, Read d)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> Asker m a -- ^'Asker' for the first parameter.
             -> Asker m b -- ^'Asker' for the second perameter.
             -> Asker m c -- ^'Asker' for the third parameter.
             -> Asker m d -- ^'Asker' for the fourth parameter.
             -> (Text -> a -> b -> c -> d -> m z)
             -> Command m (Either () z)
makeCommand4 n t d p1 p2 p3 p4 f = Command n t d (Just 4) (\inp -> checkParams n inp 4 c)
   where
      c inp = do let li = m2e "" (listToMaybe inp)
                 x1 <- onSucc li $ ask p1 (inp !! 1)
                 x2 <- onSucc x1 $ ask p2 (inp !! 2)
                 x3 <- onSucc x2 $ ask p3 (inp !! 3)
                 x4 <- onSucc x3 $ ask p4 (inp !! 4)
                 liftEM5 f li x1 x2 x3 x4


(!!) :: [a] -> Int -> Maybe a
(!!) [] _ = Nothing
(!!) (x:_) 0 = Just x
(!!) (_:xs) n = xs !! (n-1)

m2e :: Maybe a -> b -> Either b a
m2e Nothing b = Left x
m2e (Just x) _ = Right x


onSucc :: Monad m => Either a b -> m (Either a c) -> m (Either a c)
onSucc (Left y) _ = return $ Left y
onSucc (Right _) x = x



