module Web.Controller.Messages where

import Web.Controller.Prelude as Prelude
import Web.View.Messages.New
import Web.View.Messages.Show
import Web.KeyHelper
import Web.Controller.PubKeys

import OpenSSL
import OpenSSL.Cipher
import OpenSSL.EVP.Digest
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Sign
import OpenSSL.EVP.Verify
import OpenSSL.PEM
import OpenSSL.RSA
import OpenSSL.EVP.Base64

import Data.Text.Encoding
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy

import Data.Text as Text

import Data.Time.Clock
import Data.Time.Calendar

import Data.Typeable

instance Controller MessagesController where

    action NewMessageAction = do
        currentDay <- currentDayIo
        render NewView {dateToday= currentDay}
    

    action CreateMessageAction = do
        currentDay <- currentDayIo
        keyPairM <- retrieveKeyCurrentDay

        case keyPairM of
            Nothing -> do
                setErrorMessage  "Failure retrieving KeyPair for today!"
                render NewView {dateToday= currentDay}
            Just keyPair -> do
                let keyStrPem = Text.unpack(get #pem keyPair)

                case fileOrNothing2 (fileOrNothing "file") of
                    Nothing -> do
                        setErrorMessage  "Must select a valid file!"
                        render NewView {dateToday= currentDay}
                    Just file -> do

                        let contentBS :: Strict.ByteString = file
                                |> get #fileContent
                                |> toChunks
                                |> Strict.concat
                        let fileName = file |> get #fileName |> decodeUtf8

                        digest <- digestSHA
                        maybeSignatureBS <- signMessage digest keyStrPem contentBS
                        case maybeSignatureBS of
                            Nothing -> do
                                setErrorMessage  "Failure while signing file for today"
                                render NewView {dateToday= currentDay}
                            Just signatureBS -> do
                                setSuccessMessage "Message created"
                                render ShowView { signature = decodeUtf8 (encodeBase64BS signatureBS), date = currentDay, fileName= fileName}



--- Sign given digest, pem in string and KeyPair
signMessage :: Digest -> String -> Strict.ByteString -> IO (Maybe Strict.ByteString)
signMessage digest keyPairStr message = do
    someKeyPair <- readPrivateKey keyPairStr PwNone
    case toKeyPair @RSAKeyPair someKeyPair of
        Nothing -> return Nothing
        Just keyPair -> do
            signature <- signBS digest keyPair message
            return  (Just signature)

--- Validates file is has a valid fileName (fileOrNothing vies Just file even with empty file)
fileOrNothing2 :: Maybe (FileInfo Lazy.ByteString)-> Maybe (FileInfo Lazy.ByteString)
fileOrNothing2 mFile = case mFile of
        Nothing -> Nothing
        Just file -> let fileNameLength = file |> get #fileName |> decodeUtf8|> Text.length
            in if fileNameLength > 2 then Just file else Nothing



