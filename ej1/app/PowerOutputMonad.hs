module PowerOutputMonad where

import Monadas    
import Control.Monad

type PowerScreen = [ String ]
newtype PowerOutput a = O3 (a, PowerScreen)
     deriving Show

instance Functor PowerOutput where
  fmap = liftM

instance Applicative PowerOutput where
  pure x = O3 (x, [])
  (<*>) = ap

instance MonadFail PowerOutput where
  fail msg = error msg

instance Monad PowerOutput where
  m >>= k  = let O3 (v, scr1) = m
              in let O3 (res, scr2) = k v
                  in O3 (res, scr1 ++ scr2)

instance PrintMonad PowerOutput where
  printf msg = O3 ((), [ msg ])  

-- No tiene sentido hacer que PowerOutput sea instancia de ErrorMonad