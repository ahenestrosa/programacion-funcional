module Web.Controller.Messages where

import Web.Controller.Prelude
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
import Data.ByteString

import qualified Data.ByteString.Char8 as C





instance Controller MessagesController where
    action MessagesAction = do
        messages <- query @Message |> fetch
        render IndexView { .. }

    action NewMessageAction = do
        let message = newRecord
        render NewView { .. }

    action ShowMessageAction { messageId } = do
        message <- fetch messageId
        sign <- signIO
        sign2 <- signIO2
        -- verif <- verification
        render ShowView { message = message, signature = sign, signature2 = sign2, result = (VerifySuccess == VerifySuccess)}

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



signIO = rsaKey >>= \key ->
         digest1 >>= \dig ->
            sign dig key "hola"

signIO2 = rsaKey >>= \key ->
         digest1 >>= \dig ->
            sign dig key "hola"

-- verifyIO signedIO = signedIO >>= \signed ->
--                     rsaKey >>= \ key ->
--                     digest1 >>= \dig ->
--                         verifyBS dig signed key bstring

-- verification = verifyIO signIO

-- succ = VerifySuccess


bstring = C.pack "your string"