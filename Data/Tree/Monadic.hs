module Data.Tree.Monadic where

import Data.Tree as T
import Control.Monad as C

-- |A monadic rose tree in which the child nodes are wrapped in a monad.
--  @l@ is the type of the keys in the leaf nodes and @n@ the type
--  of the keys in the inner nodes.
data MTree m n = -- |An internal nodes with a value and children
                 --  wrapped in a monad.
                 MTree (m (MNode m n))

-- |A node of an 'MTree'.
data MNode m n = MNode {nodeContent::n, nodeChildren::[MTree m n]}

-- |A path in a tree.
type Path n = [n]

instance (Functor m) => Functor (MTree m) where
   fmap f (MTree m) = MTree $ fmap (fmap f) m

instance Functor m => Functor (MNode m) where
   fmap f (MNode n ns) = MNode (f n) (fmap (fmap f) ns)

-- |Completely unrolls an 'MTree' into a 'Tree', evaluating all nodes.
materialize :: Monad m => MTree m n -> m (Tree n)
materialize (MTree m) =
   do (MNode v children) <- m
      children' <- mapM materialize children
      return $ T.Node v children'

-- |Unfolds an 'MTree' from a monadic value.
--  Analogous to 'Data.Tree.unfoldTreeM'
unfoldMTree :: Monad m => (b -> m (a, [b])) -> m b -> MTree m a
unfoldMTree f x = MTree $ do (y, ys) <- x >>= f
                             return $ MNode y $ map (unfoldMTree f . return) ys
