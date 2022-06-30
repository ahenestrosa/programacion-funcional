module Web.Controller.VerificationMessages where

import Web.Controller.Prelude
import Web.View.VerificationMessages.New
import Web.View.VerificationMessages.Show

import Web.Controller.Messages 

import OpenSSL
import OpenSSL.Cipher
import OpenSSL.EVP.Digest
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Sign
import OpenSSL.EVP.Verify
import OpenSSL.PEM
import OpenSSL.RSA
import OpenSSL.EVP.Base64

import Data.Text as Text
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import Data.Text.Encoding

instance Controller VerificationMessagesController where

    action NewVerificationMessageAction = do
        render NewView {}

    action CreateVerificationMessageAction = do
        ---- TODO: Add validation of signature and file
        currentDay <- currentDayIo

        let contentBS :: Strict.ByteString = --TODO: validate, crashea si la length es incorrecta
                fileOrNothing "file"
                |> fromMaybe (error "no file given") --TODO: handle error
                |> get #fileContent
                |> toChunks
                |> Strict.concat
        
        let signatureText = param @Text "signature" --TODO: validate
        let day = param @Day "date"

        keyPairM <- retrievePubKeyByDay day
        let pubKeyStr = case keyPairM of
                Nothing -> "ERROR" -- TODO: hanlde error if pub key not present
                Just pubKey -> Text.unpack(get #pem pubKey)

        
        let signatureBS = (decodeBase64BS . encodeUtf8) signatureText

        verificationRes <- verifyMessage digestSHA pubKeyStr contentBS signatureBS

        setSuccessMessage "VerificationMessage created"
        render ShowView {result = (verificationRes == VerifySuccess)}



--- Verify given digest (io), pem as keypair, original message byte string and signature
verifyMessage :: IO Digest -> String -> Strict.ByteString -> Strict.ByteString -> IO VerifyStatus
verifyMessage digestIo keyPairStr message signature = do
    somePublicKey <- readPublicKey keyPairStr
    digest <- digestIo
    let Just publicKey = toPublicKey @RSAPubKey somePublicKey
    verifyBS digest signature publicKey message
