module ErrorMonad where

import Monadas
import Control.Monad

data Error a = Throw String | Ok a
     deriving Show
    
instance Functor Error where
  fmap = liftM

instance Applicative Error where
  pure x = Ok x
  (<*>) = ap

instance MonadFail  Error where
  fail msg = Throw msg

instance Monad Error where
  m >>= k  = case m of
               Throw s -> Throw s
               Ok v    -> k v
    
instance ErrorMonad Error where
  throw msg = Throw msg

-- No tiene sentido hacer que Error sea instancia de PrintMonad
