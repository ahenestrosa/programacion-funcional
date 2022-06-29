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

import Data.Text.Encoding

instance Controller VerificationMessagesController where

    action NewVerificationMessageAction = do
        let verificationMessage = newRecord
        render NewView { .. }

    action CreateVerificationMessageAction = do
        let verificationMessage = newRecord @VerificationMessage
        verificationMessage
            |> buildVerificationMessage
            |> ifValid \case
                Left verificationMessage -> render NewView { .. } 
                Right verificationMessage -> do
                    currentDay <- currentDayIo
                    keyPairM <- retrievePubKeyByDay currentDay
                    let pubKeyStr = case keyPairM of
                                    Nothing -> "ERROR" -- todo: hanlde error if pub key not present
                                    Just pubKey -> Text.unpack(get #pem pubKey)

                    
                    let messageBS = encodeUtf8(get #text verificationMessage)
                    let signatureBS = decodeBase64BS (encodeUtf8(get #signature verificationMessage))

                    verificationRes <- verifyMessage digestSHA pubKeyStr messageBS signatureBS

                    setSuccessMessage "VerificationMessage created"
                    render ShowView {verificationMessage = verificationMessage, result = (verificationRes == VerifySuccess)}



buildVerificationMessage verificationMessage = verificationMessage
    |> fill @["text","signature","date"]


--- Verify given digest (io), pem as keypair, original message and signature
verifyMessage :: IO Digest -> String -> ByteString -> ByteString -> IO VerifyStatus
verifyMessage digestIo keyPairStr message signature = do
    somePublicKey <- readPublicKey keyPairStr
    digest <- digestIo
    let Just publicKey = toPublicKey @RSAPubKey somePublicKey
    verifyBS digest signature publicKey message
