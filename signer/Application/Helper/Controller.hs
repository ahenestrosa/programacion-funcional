module Application.Helper.Controller where

import IHP.ControllerPrelude


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
import Data.ByteString as BS



-- Here you can add functions which are available in all your controllers


---- Digest for hashing of sign and verify.
digestSHA = getDigestByName "SHA256" >>= (\md -> let Just d = md in return d)


dateGregorianIo :: IO (Integer, Int, Int) -- :: (year, month, day)
dateGregorianIo = getCurrentTime >>= return . toGregorian . utctDay

currentDayIo :: IO Day
currentDayIo = getCurrentTime >>= return . utctDay
