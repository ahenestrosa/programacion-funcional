module Web.Controller.Messages where

import Web.Controller.Prelude as Prelude
import Web.View.Messages.New
import Web.View.Messages.Show
import Web.Controller.Key

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

import qualified Data.ByteString.Char8 as C

import Data.Text as Text

import Data.Time.Clock
import Data.Time.Calendar

import Data.Typeable

instance Controller MessagesController where

    action NewMessageAction = do
        let message = newRecord
        render NewView { .. }

    action CreateMessageAction = do
        let message = newRecord @Message
        message
            |> buildMessage
            |> ifValid \case
                Left message -> render NewView { .. } 
                Right message -> do
                    currentDay <- currentDayIo
                    keyPairM <- retrieveKeyCurrentDay
                    let keyStr = case keyPairM of
                                    Nothing -> "ERROR" -- todo: hanlde error
                                    Just keyPair -> Text.unpack(get #pem keyPair)

                    let messageStr = Text.unpack(get #text message)

                    -- let date = get #date keyObj

                    signature <- signMessage digestSHA keyStr messageStr
                    verificationRes <- verifyMessage digestSHA keyStr messageStr signature
                    currDay <- currentDayIo
                    setSuccessMessage "Message created"
                    render ShowView { message = message, signature = keyStr, signature2 = signature, result = verificationRes == VerifySuccess}


buildMessage message = message
    |> fill @'["text"]





--- Sign given digest, pem in string and KeyPair
signMessage :: IO Digest -> String -> String -> IO String
signMessage digestIo keyPairStr message = do
    someKeyPair <- readPrivateKey keyPairStr PwNone
    digest <- digestIo
    let Just keyPair = toKeyPair @RSAKeyPair someKeyPair
    signature <- sign digest keyPair message
    return signature


--- Verify given digest (io), pem as keypair, original message and signature
verifyMessage :: IO Digest -> String -> String -> String -> IO VerifyStatus
verifyMessage digestIo keyPairStr message signature = do
    someKeyPair <- readPrivateKey keyPairStr PwNone
    digest <- digestIo
    let Just keyPair = toKeyPair @RSAKeyPair someKeyPair
        in verify digest signature keyPair message



--- Retrieve keyPair for current day, if non existent it generates the key pair
retrieveKeyCurrentDay :: (?modelContext :: ModelContext) => IO (Maybe Key)
retrieveKeyCurrentDay = do
    currentDay <- currentDayIo
    keyPairM <- retrieveKeyByDay currentDay
    case keyPairM of
        --- If exists for current day just return it
        Just keyPair -> return (Just keyPair)
        --- If doesnt exists for current day, generate it
        Nothing -> do
            newKeyPair <- generateKeyPairToday
            newKeyPair |> createRecord
            return (Just newKeyPair)


--- Retrieves KeyPair for an specific day
retrieveKeyByDay :: (?modelContext :: ModelContext) => Day -> IO (Maybe Key)
retrieveKeyByDay day = do
    result :: [Key] <- sqlQuery "SELECT * FROM key WHERE date = ?" (Only day)
    return (Prelude.head result) 


