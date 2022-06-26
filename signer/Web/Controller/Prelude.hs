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

-- import qualified Codec.Crypto.RSA as Crypto
-- import "crypto-api" Crypto.Random

import OpenSSL
import OpenSSL.BN
import OpenSSL.Cipher
import OpenSSL.DER
import OpenSSL.DH
import OpenSSL.DSA
import OpenSSL.EVP.Base64
import OpenSSL.EVP.Cipher
import OpenSSL.EVP.Digest
import OpenSSL.EVP.Internal
import OpenSSL.EVP.Open
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Seal
import OpenSSL.EVP.Sign
import OpenSSL.EVP.Verify
import OpenSSL.PEM
import OpenSSL.PKCS7
import OpenSSL.RSA
import OpenSSL.Random
import OpenSSL.Session

import OpenSSL.EVP.Digest



-- someFunc = do
--   g <- newGenIO :: IO SystemRandom
--   let keyPair = Crypto.generateKeyPair g 10
--   let (pubKey, privKey, randG) = keyPair
--     in return (Crypto.public_size pubKey)

rsaKey = generateRSAKey 1024 17 Nothing

-- sign = signBS @RSAKeyPair rsaKey


digest1 = getDigestByName "SHA256" >>= (\md -> let Just d = md in return d)

signIO = rsaKey >>= \key ->
         digest1 >>= \dig ->
            return (signBS dig key "hola")

-- pKey = newPKeyRSA rsaKey

