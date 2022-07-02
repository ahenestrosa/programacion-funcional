module Web.Controller.VerificationMessages where

import Web.Controller.Prelude as Prelude
import Web.View.VerificationMessages.New
import Web.View.VerificationMessages.Show
import Web.Controller.Messages

import Web.KeyHelper

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
        currentDay <- currentDayIo
        let maybeDay = paramOrNothing @Day "date"
        case maybeDay of
            Nothing -> do
                setErrorMessage  "Must select a valid day"
                render NewView {}
            Just day -> do
                keyPairM <- retrievePubKeyByDay day
                case keyPairM of
                    Nothing -> do
                        setErrorMessage  "Public key not present for selected day"
                        render NewView {}

                    Just pubKey -> do
                        let pubKeyPem = Text.unpack(get #pem pubKey)

                        case fileOrNothing2 (fileOrNothing "file") of
                            Nothing -> do
                                setErrorMessage  "Must select a valid file!"
                                render NewView {}
                            Just file -> do

                                let contentBS :: Strict.ByteString = file
                                        |> get #fileContent
                                        |> toChunks
                                        |> Strict.concat
                                let fileName = file |> get #fileName |> decodeUtf8
                                
                                let signatureBSOrError = getSignatureAsBS (param @Text "signature")
                                
                                case signatureBSOrError of
                                    Right errorMessage -> do
                                        setErrorMessage errorMessage
                                        render NewView {}
                                    Left signatureBS -> do

                                        verificationRes <- verifyMessage digestSHA pubKeyPem contentBS signatureBS
                                        setSuccessMessage "VerificationMessage created"
                                        render ShowView {result = (verificationRes == VerifySuccess)}



--- Verify given digest (io), pem as keypair, original message byte string and signature
verifyMessage :: IO Digest -> String -> Strict.ByteString -> Strict.ByteString -> IO VerifyStatus
verifyMessage digestIo keyPairStr message signature = do
    somePublicKey <- readPublicKey keyPairStr
    digest <- digestIo
    let Just publicKey = toPublicKey @RSAPubKey somePublicKey
    verifyBS digest signature publicKey message


getSignatureAsBS :: Text -> Either Strict.ByteString Text 
getSignatureAsBS signatureText = 
    case validateLenght 172 signatureText of
        False -> Right "Invalid signature: Incorrect size"
        True -> Left (decodeBase64BS  (encodeUtf8 signatureText))


validateLenght :: Int -> Text -> Bool
validateLenght len = (==len) . Text.length