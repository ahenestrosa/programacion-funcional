module RevOutputMonad where

import Monadas    
import Control.Monad

type Screen = String    
newtype RevOutput a = O2 (Screen, a)
     deriving Show

instance Functor RevOutput where
  fmap = liftM

instance Applicative RevOutput where
  pure x = O2 ("", x)
  (<*>) = ap

instance MonadFail  RevOutput where
  fail msg = error msg

instance Monad RevOutput where
  m >>= k  = let O2 (scr1, v) = m
              in let O2 (scr2, res) = k v
                  in O2 (scr2 ++ scr1, res)

instance PrintMonad RevOutput where
  printf msg = O2 (msg ++ "\n", ())  

-- No tiene sentido hacer que RevOutput sea instancia de ErrorMonad