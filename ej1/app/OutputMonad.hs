module OutputMonad where

import Monadas    
import Control.Monad

type Screen = String    
newtype Output a = O (a, Screen)
     deriving Show

instance Functor Output where
  fmap = liftM

instance Applicative Output where
  pure x = O (x, "")
  (<*>) = ap

instance MonadFail  Output where
  fail msg = error msg

instance Monad Output where
  m >>= k  = let O (v, scr1) = m
              in let O (res, scr2) = k v
                  in O (res, scr1 ++ scr2)

instance PrintMonad Output where
  printf msg = O ((), msg ++ "\n")  

-- No tiene sentido hacer que Output sea instancia de ErrorMonad