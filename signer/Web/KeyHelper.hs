module Web.KeyHelper where

import Web.Controller.Prelude as Prelude

import OpenSSL
import OpenSSL.Cipher
import OpenSSL.EVP.Digest
import OpenSSL.EVP.PKey
import OpenSSL.EVP.Sign
import OpenSSL.EVP.Verify
import OpenSSL.PEM
import OpenSSL.RSA
import OpenSSL.EVP.Base64
import Data.ByteString

import Data.Text as Text

import qualified Data.ByteString.Char8 as C

--Generates  RSA KeyPair + Public Key  for today
generateKeyPairToday :: IO (Key, PubKey)
generateKeyPairToday = do
    currentDay <- currentDayIo
    rsaKey <- generateRSAKey 1024 17 Nothing
    pubKey <- rsaCopyPublic rsaKey

    rsaKeyPem <- writePKCS8PrivateKey rsaKey Nothing
    rsaPubKeyPem <- writePublicKey pubKey

    let newKey = newRecord @Key |> set #date currentDay |> set #pem (Text.pack rsaKeyPem)
    let newPubKey = newRecord @PubKey |> set #date currentDay |> set #pem (Text.pack rsaPubKeyPem)
        in return (newKey, newPubKey)

    
--- Retrieve keyPair for current day, if non existent it generates the key pair
--- and inserts corresponding KeyPair and PublicKey in database
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


--- Deletes all key pairs older than certain date
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


---- Digest for hashing of sign and verify.
digestSHA = getDigestByName "SHA256" >>= (\md -> let Just d = md in return d)



