module MiFalla where

-- Una alternativa a Maybe, para mostrar que puedo (no utilizada en Main)

import Monadas
import Control.Monad
  
data MiFalla a = Falla | Anda a
     deriving Show

instance Functor MiFalla where
  fmap = liftM

instance Applicative MiFalla where
  pure x = Anda x
  (<*>) = ap

instance MonadFail  MiFalla where
  fail msg = Falla

instance Monad MiFalla where 
  m >>= k = case m of
               Falla -> Falla 
               Anda v -> k v

-- No tiene sentido hacer que MiFalla sea instancia de PrintMonad
