module Web.Controller.Messages where

import Web.Controller.Prelude as Prelude
import Web.View.Messages.New
import Web.View.Messages.Show
import Web.Controller.Key
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
        render NewView {}

    -- action CreateMessageAction = do
    --     let message = newRecord @Message
    --     message
    --         |> buildMessage
    --         |> ifValid \case
    --             Left message -> render NewView { .. } 
    --             Right message -> do
    --                 currentDay <- currentDayIo
    --                 keyPairM <- retrieveKeyCurrentDay
    --                 let keyStr = case keyPairM of
    --                                 Nothing -> "ERROR" -- todo: hanlde error
    --                                 Just keyPair -> Text.unpack(get #pem keyPair)

    --                 let messageBS = encodeUtf8 (get #text message)

    --                 signatureBS <- signMessage digestSHA keyStr messageBS
    --                 setSuccessMessage "Message created"
    --                 render ShowView { message = message, signature = decodeUtf8 messageBS, signature2 = decodeUtf8 (encodeBase64BS signatureBS)}

    action CreateMessageAction = do
        currentDay <- currentDayIo
        keyPairM <- retrieveKeyCurrentDay

        let contentBS :: Strict.ByteString =
                fileOrNothing "file"
                |> fromMaybe (error "no file given")
                |> get #fileContent
                |> toChunks
                |> Strict.concat


        let keyStr = case keyPairM of
                        Nothing -> "ERROR" -- todo: hanlde error
                        Just keyPair -> Text.unpack(get #pem keyPair)

        -- let messageBS = encodeUtf8 (messageText)

        signatureBS <- signMessage digestSHA keyStr contentBS
        setSuccessMessage "Message created"
        render ShowView { signature = decodeUtf8 (encodeBase64BS signatureBS), date = currentDay}

-- TODO: ADD VALIDATION


buildMessage message = message
    |> fill @'["text"]





--- Sign given digest, pem in string and KeyPair
signMessage :: IO Digest -> String -> Strict.ByteString -> IO Strict.ByteString
signMessage digestIo keyPairStr message = do
    someKeyPair <- readPrivateKey keyPairStr PwNone
    digest <- digestIo
    let Just keyPair = toKeyPair @RSAKeyPair someKeyPair
    signature <- signBS digest keyPair message
    return signature


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
            generated <- generateKeyPairToday
            let (newKeyPair, newPubKey) = generated
            newKeyPair |> createRecord
            newPubKey |> createRecord

            -- Delete all older KeyPairs than today. PubKeys are persisted
            deleteKeyPairsOlderThan currentDay

            return (Just newKeyPair)


deleteKeyPairsOlderThan :: (?modelContext :: ModelContext) => Day -> IO ()
deleteKeyPairsOlderThan day = do
    olderKeys :: [Key] <- sqlQuery "SELECT * FROM key where date < ?" (Only day)
    deleteRecords olderKeys


--- Retrieves  KeyPair for an specific day
retrieveKeyByDay :: (?modelContext :: ModelContext) => Day -> IO (Maybe Key)
retrieveKeyByDay day = do
    result :: [Key] <- sqlQuery "SELECT * FROM key WHERE date = ?" (Only day)
    return (Prelude.head result) 


--- Retrieves PubKey for an specific day
retrievePubKeyByDay :: (?modelContext :: ModelContext) => Day -> IO (Maybe PubKey)
retrievePubKeyByDay day = do
    result :: [PubKey] <- sqlQuery "SELECT * FROM pub_keys WHERE date = ?" (Only day)
    return (Prelude.head result) 





