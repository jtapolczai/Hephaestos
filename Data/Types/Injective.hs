{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Types.Injective where

import qualified Numeric.Natural as N
import qualified Numeric.Natural.Internal as N (runNatural)
import Data.Default
import qualified Data.Maybe as M
import qualified Data.Ratio as R
import qualified Data.Text as T
import qualified Data.ByteString as B

-- |The class relation between types @a@ and @b@ s.t. @a@ can be injected
--  into @b@.
--
--  The following laws must be fulfilled:
--  * 'to' is total.
--  * @x /= y  ==>  (to x) /= (to y)@.
class Injective a b where
   to :: a -> b

instance Injective a a where
   to = id

instance Injective T.Text String where to = T.unpack
instance Injective String T.Text where to = T.pack
instance Injective N.Natural Integer where to = N.runNatural
instance Injective Integer R.Rational where to = flip (R.%) 1
instance Default a => Injective (Maybe b) (Either a b) where
   to = M.maybe (Left def) Right

