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





instance Controller MessagesController where
    action MessagesAction = do
        messages <- query @Message |> fetch
        render IndexView { .. }

    action NewMessageAction = do
        let message = newRecord
        render NewView { .. }

    action ShowMessageAction { messageId } = do
        message <- fetch messageId
        keys <- query @Key |> fetch
        let maybeKeyObj =  Prelude.head keys
        let Just keyObj = maybeKeyObj
        let keyStr = Text.unpack(get #pem keyObj)
        somePubKey <- readPrivateKey keyStr PwNone
        let Just publicKey = (toKeyPair @RSAKeyPair (somePubKey))
        digest <- digest1
        signature <- sign digest publicKey "hola"
        render ShowView { message = message, signature = keyStr, signature2 = signature, result = True}

    -- action ShowMessageAction { messageId } = do
    --     message <- fetch messageId
    --     keyPairStr <- keyPairString
    --     somePubKey <- readPrivateKey keyPairStr PwNone
    --     let Just publicKey = (toKeyPair @RSAKeyPair (somePubKey))
    --     digest <- digest1
    --     signature <- sign digest publicKey "hola"
    --     render ShowView { message = message, signature = keyPairStr, signature2 = signature, result = True}

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


rsaKey = generateRSAKey 1024 17 Nothing

digest1 = getDigestByName "SHA256" >>= (\md -> let Just d = md in return d)

keyPairString = rsaKey >>= \key ->
                    writePublicKey @RSAKeyPair key



signIO = rsaKey >>= \key ->
         digest1 >>= \dig ->
            sign dig key "hola"


-- verifyIO signedIO = signedIO >>= \signed ->
--                     rsaKey >>= \ key ->
--                     digest1 >>= \dig ->
--                         verifyBS dig signed key bstring

-- verification = verifyIO signIO

-- succ = VerifySuccess



bstring = C.pack "your string"