module Web.Controller.Messages where

import Web.Controller.Prelude as Prelude
import Web.View.Messages.Index
import Web.View.Messages.New
import Web.View.Messages.Edit
import Web.View.Messages.Show

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
    action MessagesAction = do
        messages <- query @Message |> fetch
        render IndexView { .. }

    action NewMessageAction = do
        let message = newRecord
        render NewView { .. }

    action ShowMessageAction { messageId } = do
        message <- fetch messageId
        currentDay <- currentDayIo
        keyPair <- retrieveKeyByDay currentDay


        let keyStr = Text.unpack(get #pem keyPair)

        let messageStr = Text.unpack(get #text message)

        -- let date = get #date keyObj

        signature <- signMessage digestSHA keyStr messageStr
        verificationRes <- verifyMessage digestSHA keyStr messageStr signature
        currDay <- currentDayIo

        render ShowView { message = message, signature = keyStr, signature2 = signature, result = verificationRes == VerifySuccess}

    action EditMessageAction { messageId } = do
        message <- fetch messageId
        render EditView { .. }

    action UpdateMessageAction { messageId } = do
        message <- fetch messageId
        message
            |> buildMessage
            |> ifValid \case
                Left message -> render EditView { .. }
                Right message -> do
                    message <- message |> updateRecord
                    setSuccessMessage "Message updated"
                    redirectTo EditMessageAction { .. }

    action CreateMessageAction = do
        let message = newRecord @Message
        message
            |> buildMessage
            |> ifValid \case
                Left message -> render NewView { .. } 
                Right message -> do
                    message <- message |> createRecord
                    setSuccessMessage "Message created"
                    redirectTo MessagesAction

    action DeleteMessageAction { messageId } = do
        message <- fetch messageId
        deleteRecord message
        setSuccessMessage "Message deleted"
        redirectTo MessagesAction

buildMessage message = message
    |> fill @'["text"]


digestSHA = getDigestByName "SHA256" >>= (\md -> let Just d = md in return d)


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



--- Retrieves for an specific day
retrieveKeyByDay :: (?modelContext :: ModelContext) => Day -> IO Key
retrieveKeyByDay day = do
    result :: [Key] <- sqlQuery "SELECT * FROM key WHERE date = ?" (Only day)
    let Just key = (Prelude.head result)
        in return key

dateGregorianIo :: IO (Integer, Int, Int) -- :: (year, month, day)
dateGregorianIo = getCurrentTime >>= return . toGregorian . utctDay

currentDayIo = dateGregorianIo >>= \dateGreg ->
    let (year, month, day) = dateGreg
        in return (fromGregorian year month day)
