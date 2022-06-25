module Web.Controller.Prelude
( module Web.Types
, module Application.Helper.Controller
, module IHP.ControllerPrelude
, module Generated.Types
)
where

import Web.Types
import Application.Helper.Controller
import IHP.ControllerPrelude
import Generated.Types
import Web.Routes

import qualified Codec.Crypto.RSA as Crypto
import "crypto-api" Crypto.Random

someFunc = do
  g <- newGenIO :: IO SystemRandom
  let keyPair = Crypto.generateKeyPair g 10
    in return keyPair


