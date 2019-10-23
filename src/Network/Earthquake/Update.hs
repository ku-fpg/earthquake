module Network.Earthquake.Update where

import Data.Bifunctor

class External m where
  external :: Monoid a => a -> m a ()

instance External (,) where
  external m = (m,())

class Context m where
   context :: (s -> m s a b) -> m s a b

-- Alternatives to 2-tuples for update.

newtype Pure a b = Pure b

runPure :: Monoid a => Pure a b -> (a,b)
runPure (Pure b) = (mempty,b)

instance Functor (Pure a) where
  fmap f (Pure b) = Pure (f b)

instance Applicative (Pure a) where
  pure = Pure
  Pure f <*> Pure g = Pure (f g)

instance Bifunctor Pure where
  bimap _ g (Pure a) = Pure (g a)

newtype Reader s a b = Reader (s -> (a,b))

runReader :: s -> Reader s a b -> (a,b)
runReader s (Reader f) = f s

instance Functor (Reader s a) where
  fmap f (Reader g) = Reader (fmap f . g)

instance Monoid a => Applicative (Reader s a) where
  pure = Reader . const . pure 
  Reader f <*> Reader g = Reader $ \ s -> f s <*> g s

instance Bifunctor (Reader s) where
  bimap f g (Reader h) = Reader (bimap f g . h)

instance External (Reader s) where
  external = Reader . const . external

instance Context Reader where
  context k = Reader $ \ s -> case k s of
    Reader f -> f s
