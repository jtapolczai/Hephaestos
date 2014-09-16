{-# LANGUAGE OverloadedStrings #-}

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
   PredicateAsker,
   PredicateChecker,
   prompt,
   prompt',
   askFor,
   askForWhen,
   askOnceWhen,
   checkWhen,
   -- *Convenience functions for handling state
   -- These can be convenient when one wishes to 
   -- to extract a number of elements from the current state via pattern
   -- -matching, e.g.
   --
   -- @
   -- data State = State{f::a,g::b,h::c}
   --
   -- do (x,z) <- get2 f h
   --    ...do something with x and z...
   -- @
   get1,
   get2,
   get3,
   get4,
   get5,
   get6,
   get7,
   get8,
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

import Prelude hiding (putStrLn, putStr, getLine, unwords, words)
import qualified Prelude as P

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Text
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

-- |Asks the user for an input of a given type.
--  If the input is of the wrong type, an error-message is printed
--  and the user is asked again.
--  This function is intended to be used like this:
--
--  @
--  (var::Int) <- askFor prompt errorMessage
--  @
askFor :: (MonadIO m, Read a)
       => Text -- ^The prompt.
       -> Text -- ^The error message in case the read of the input fails.
       -> m a
askFor pr err = askForWhen pr err undefined (const $ return True)

-- |An ask function which asks the user for an input
--  and prints an error if it doesn't satisfy certain
--  conditions.
type PredicateAsker m a r = Text -- ^The prompt.
                            -> PredicateChecker m a r

-- |The core of 'PredicateAsker' which just has the checks,
--  not the input reading.
type PredicateChecker m a r = Text -- ^The error message in case the read of the input fails.
                            -> Text -- ^The error message in case the read succeeds but the
                                    -- ^predicate returns false.
                            -> (a -> m Bool) -- ^The predicate which the input must fulfill.
                            -> m r

-- |Represents a failure of a ask function.
--  It can either be a type failure (failure to interpret the
--  user input as a value of the required type) or a predicate
--  failure (the user input could be interpreter as a value
--  of the required type, but it failed some user-supplied test).
data AskFailure = TypeFailure Text | PredicateFailure Text

-- |Asks the user for an input of a given type.
--  If the input is of the wrong type, an error-message is printed
--  and the user is asked again.
--  In addition to the condition that the input must be of the correct
--  type, it must also fulfill a predicate.
--
--  Since the predicate is of type @a -> m Bool@, arbitrarily complex
--  tests can be performed: checking whether an item is in a database,
--  whether a date was less than x years ago, etc.
askForWhen :: (MonadIO m, Read a) => PredicateAsker m a a
askForWhen pr errType errCheck check =
   let
      rec = askForWhen pr errType errCheck check
   in
   do y <- liftIO $ prompt' pr
      let x = readMaybe $ unpack y
      case x of Nothing -> putErrLn errType >> rec
                Just x' -> do res <- check x'
                              if res then return x'
                              else putErrLn errCheck >> rec

-- |The same as 'askForWhen', but only asks the user once.
askOnceWhen :: (MonadIO m, Read a) => PredicateAsker m a (Either AskFailure a)
askOnceWhen pr errType errCheck check =
   do y <- liftIO $ prompt' pr
      checkWhen y errType errCheck check

-- |Takes user input, tries to read it, and checks whether
--  it fulfils a given predicate. Unlike 'askFor', 'askForWhen' and
--  'askOnceWhen', it doesn't read the input from stdio itself.
checkWhen :: (MonadIO m, Read a) => Text -> PredicateChecker m a (Either AskFailure a)
checkWhen y errType errCheck check =
   let x = readMaybe $ unpack y
   in 
      case x of Nothing -> return $ Left $ TypeFailure errType
                Just x' -> do res <- check x'
                              return $ if res then Right x'
                                       else Left $ PredicateFailure errCheck

-- |Extracts a result from the current state.
--  Defined as @get1 f = liftM f get@.
get1 :: Monad m
     => (s -> a)
     -> StateT s m a
get1 f1 = liftM f1 get

-- |Extracts two results from the current state.
get2 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> StateT s m (a,b)
get2 f1 f2 = liftM (f1 &&& f2) get

-- |Extracts three results from the current state.
get3 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> StateT s m (a,b,c)
get3 f1 f2 f3 = liftM (\x -> (f1 x,f2 x, f3 x)) get

-- |Extracts four results from the current state.
get4 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> StateT s m (a,b,c,d)
get4 f1 f2 f3 f4 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x)) get

-- |Extracts five results from the current state.
get5 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> StateT s m (a,b,c,d,e)
get5 f1 f2 f3 f4 f5 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x)) get

-- |Extracts six results from the current state.
get6 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> (s -> f)
     -> StateT s m (a,b,c,d,e,f)
get6 f1 f2 f3 f4 f5 f6 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x, f6 x)) get

-- |Extracts seven results from the current state.
get7 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> (s -> f)
     -> (s -> g)
     -> StateT s m (a,b,c,d,e,f,g)
get7 f1 f2 f3 f4 f5 f6 f7 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x, f6 x, f7 x)) get

-- |Extracts eight results from the current state.
get8 :: Monad m
     => (s -> a)
     -> (s -> b)
     -> (s -> c)
     -> (s -> d)
     -> (s -> e)
     -> (s -> f)
     -> (s -> g)
     -> (s -> h)
     -> StateT s m (a,b,c,d,e,f,g,h)
get8 f1 f2 f3 f4 f5 f6 f7 f8 = liftM (\x -> (f1 x,f2 x, f3 x, f4 x, f5 x, f6 x, f7 x, f8 x)) get

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
                   putStrLn $ maybe "" (pack . (" Parameters: " ++) . show) (numParameters c)
                   putStrLn $ commandDesc c

-- |Splits and cleans the input of a command.
--  Be aware that this is not a security-related function!
--  Defined as @map strip . words . strip@.
sanitize :: Text -> [Text]
sanitize = Prelude.map strip . words . strip

-- |Takes a line of text and a command.
--  If the text matches the given command's 'commandTest',
--  the command is run with it. If not, the text is returned.
--
--  Example usage: @runOnce getLine command@
runOnce :: MonadIO m => m Text -> Command m a -> m (Either a Text)
runOnce l c = do l' <- l
                 if commandTest c l' then liftM Left (runCommand c l')
                                     else return $ Right l'

-- |Prints an error messages to stderr that a command with no parameters
--  was given @n@.
noParamErr :: MonadIO m
           => Text -- ^The command name.
           -> Text -- ^The given input
           -> m ()
noParamErr c inp = putErrLn $ "Parameters '" `append` unwords (sanitize inp)
                              `append` "' given to '" `append` c `append` "'. "
                              `append` c `append` " takes no parameters."

paramErr :: MonadIO m
         => Text -- ^The command name.
         -> Text -- ^The given input.
         -> Int  -- ^The expected number of parameters.
         -> m ()
paramErr c inp n = putErrLn $ l `append` " parameters given to '" `append` c
                              `append` "'. " `append` c `append` " takes "
                              `append` n' `append` " parameters."
   where l = pack.show.Prelude.length.sanitize $ inp
         n' = pack.show $ n



askIfMissing :: (MonadIO m, Read a) => Int -> [Text]-> PredicateAsker m a (Either AskFailure a)
askIfMissing i xs pr eT eP p
   | Prelude.length xs >= i = checkWhen (xs !! i) eT eP p
   | otherwise              = askOnceWhen pr eT eP p

-- |Creates a command without parameters.
makeCommand :: MonadIO m
            => Text -- ^Command name.
            -> (Text -> Bool) -- ^Command test.
            -> Text -- ^Command description.
            -> m a -- ^The actual command.
            -> Command m (Either () a)
makeCommand n t d c = Command n t d (Just 0) c'
   where c' inp = if Prelude.length (sanitize inp) > 1 then
                     noParamErr n inp >> return (Left ())
                  else
                     liftM Right c

makeCommand1 :: (MonadIO m, Read a)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> (Text, Text, Text, a -> m Bool) -- ^Parameters for reading the first parameter. See 'PredicateAsker'.
             -> (a -> m z)
             -> Command m (Either () z)
makeCommand1 n t d p1 f = Command n t d (Just 1) c'
   where
      c' inp = if Prelude.length (sanitize inp) > 2 then
                  paramErr n inp 1 >> return (Left ())
               else c'' (sanitize inp)

      c'' inp = do x1 <- curry4 (askIfMissing 1 inp) p1
                   let x1' = fromRight x1
 
                   onEither x1 (return $ Left ())
                            (liftM Right $ f x1')

makeCommand2 :: (MonadIO m, Read a, Read b)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> (Text, Text, Text, a -> m Bool) -- ^Parameters for reading the first parameter. See 'PredicateAsker'.
             -> (Text, Text, Text, b -> m Bool) -- ^Parameters for reading the second parameter. See 'PredicateAsker'.             -> (a -> b -> c -> m z)
             -> (a -> b -> m z)
             -> Command m (Either () z)
makeCommand2 n t d p1 p2 f = Command n t d (Just 2) c'
   where
      c' inp = if Prelude.length (sanitize inp) > 3 then
                  paramErr n inp 2 >> return (Left ())
               else c'' (sanitize inp)

      c'' inp = do x1 <- curry4 (askIfMissing 1 inp) p1
                   x2 <- onSucc x1 $ curry4 (askIfMissing 2 inp) p2
                   let x1' = fromRight x1
                       x2' = fromRight x2
 
                   onEither x2 (return $ Left ())
                            (liftM Right $ f x1' x2')

makeCommand3 :: (MonadIO m, Read a, Read b, Read c)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> (Text, Text, Text, a -> m Bool) -- ^Parameters for reading the first parameter. See 'PredicateAsker'.
             -> (Text, Text, Text, b -> m Bool) -- ^Parameters for reading the second parameter. See 'PredicateAsker'.
             -> (Text, Text, Text, c -> m Bool) -- ^Parameters for reading the third parameter. See 'PredicateAsker'.
             -> (a -> b -> c -> m z)
             -> Command m (Either () z)
makeCommand3 n t d p1 p2 p3 f = Command n t d (Just 3) c'
   where
      c' inp = if Prelude.length (sanitize inp) > 4 then
                  paramErr n inp 3 >> return (Left ())
               else c'' (sanitize inp)

      c'' inp = do x1 <- curry4 (askIfMissing 1 inp) p1
                   x2 <- onSucc x1 $ curry4 (askIfMissing 2 inp) p2
                   x3 <- onSucc x2 $ curry4 (askIfMissing 3 inp) p3
                   let x1' = fromRight x1
                       x2' = fromRight x2
                       x3' = fromRight x3
 
                   onEither x3 (return $ Left ())
                            (liftM Right $ f x1' x2' x3')

makeCommand4 :: (MonadIO m, Read a, Read b, Read c, Read d)
             => Text -- ^Command name.
             -> (Text -> Bool) -- ^Command test.
             -> Text -- ^Command description
             -> (Text, Text, Text, a -> m Bool) -- ^Parameters for reading the first parameter. See 'PredicateAsker'.
             -> (Text, Text, Text, b -> m Bool) -- ^Parameters for reading the second parameter. See 'PredicateAsker'.
             -> (Text, Text, Text, c -> m Bool) -- ^Parameters for reading the third parameter. See 'PredicateAsker'.
             -> (Text, Text, Text, d -> m Bool) -- ^Parameters for reading the fourth parameter. See 'PredicateAsker'.
             -> (a -> b -> c -> d -> m z)
             -> Command m (Either () z)
makeCommand4 n t d p1 p2 p3 p4 f = Command n t d (Just 4) c'
   where
      c' inp = if Prelude.length (sanitize inp) > 5 then
                  paramErr n inp 4 >> return (Left ())
               else c'' (sanitize inp)

      c'' inp = do x1 <- curry4 (askIfMissing 1 inp) p1
                   x2 <- onSucc x1 $ curry4 (askIfMissing 2 inp) p2
                   x3 <- onSucc x2 $ curry4 (askIfMissing 3 inp) p3
                   x4 <- onSucc x2 $ curry4 (askIfMissing 4 inp) p4
                   let x1' = fromRight x1
                       x2' = fromRight x2
                       x3' = fromRight x3
                       x4' = fromRight x4
 
                   onEither x4 (return $ Left ())
                            (liftM Right $ f x1' x2' x3' x4')














onSucc :: Monad m => Either a b -> m (Either a c) -> m (Either a c)
onSucc (Left y) x = return $ Left y
onSucc (Right y) x = x

onEither :: Either a b -> c -> c -> c
onEither (Left _) l _ = l
onEither (Right _) _ r = r

fromRight :: Either a b -> b
fromRight (Right r) = r

curry4 :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
curry4 f (x,y,z,u) = f x y z u


