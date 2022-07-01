module Web.Controller.Key where

import Web.Controller.Prelude as Prelude
import Web.View.Key.Index
import Web.View.Key.New
import Web.View.Key.Edit
import Web.View.Key.Show

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

import Data.Text as Text

import qualified Data.ByteString.Char8 as C

instance Controller KeyController where
    action KeysAction = do
        key <- query @Key |> fetch
        render IndexView { .. }

    action NewKeyAction = do
        let key = newRecord
        render NewView { .. }

    action ShowKeyAction { keyId } = do
        key <- fetch keyId
        render ShowView { .. }

    action EditKeyAction { keyId } = do
        key <- fetch keyId
        render EditView { .. }

    action UpdateKeyAction { keyId } = do
        key <- fetch keyId
        key
            |> buildKey
            |> ifValid \case
                Left key -> render EditView { .. }
                Right key -> do
                    key <- key |> updateRecord
                    setSuccessMessage "Key updated"
                    redirectTo EditKeyAction { .. }

    action CreateKeyAction = do
        generated <- generateKeyPairToday
        let (key, _) = generated
        key
            |> ifValid \case
                Left key -> render NewView { .. } 
                Right key -> do
                    key <- key |> createRecord
                    setSuccessMessage "Key created"
                    redirectTo KeysAction

    action DeleteKeyAction { keyId } = do
        key <- fetch keyId
        deleteRecord key
        setSuccessMessage "Key deleted"
        redirectTo KeysAction

buildKey key = key
    |> fill @["pem","date"]

rsaKey = generateRSAKey 1024 17 Nothing

keyPairString = rsaKey >>= \key ->
                   writePKCS8PrivateKey key Nothing


-- --- Generates key for today
generateKeyPairToday :: IO (Key, PubKey)
generateKeyPairToday = do
    currentDay <- currentDayIo
    rsaKey <- generateRSAKey 1024 17 Nothing
    pubKey <- rsaCopyPublic rsaKey

    rsaKeyPem <- writePKCS8PrivateKey rsaKey Nothing
    rsaPubKeyPem <- writePublicKey pubKey

    let newKey = Key {id = def, date = currentDay, pem = Text.pack rsaKeyPem, meta = MetaBag {annotations = [], touchedFields = [], originalDatabaseRecord = Nothing}}
    let newPubKey = PubKey {id = def, date = currentDay, pem = Text.pack rsaPubKeyPem, meta = MetaBag {annotations = [], touchedFields = [], originalDatabaseRecord = Nothing}}
        in return (newKey, newPubKey)

    
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



