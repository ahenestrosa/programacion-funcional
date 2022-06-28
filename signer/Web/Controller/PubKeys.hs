module Web.Controller.PubKeys where

import Web.Controller.Prelude
import Web.View.PubKeys.Index
import Web.View.PubKeys.New
import Web.View.PubKeys.Edit
import Web.View.PubKeys.Show

instance Controller PubKeysController where
    action PubKeysAction = do
        pubKeys <- query @PubKey |> fetch
        render IndexView { .. }

    action NewPubKeyAction = do
        let pubKey = newRecord
        render NewView { .. }

    action ShowPubKeyAction { pubKeyId } = do
        pubKey <- fetch pubKeyId
        render ShowView { .. }

    action EditPubKeyAction { pubKeyId } = do
        pubKey <- fetch pubKeyId
        render EditView { .. }

    action UpdatePubKeyAction { pubKeyId } = do
        pubKey <- fetch pubKeyId
        pubKey
            |> buildPubKey
            |> ifValid \case
                Left pubKey -> render EditView { .. }
                Right pubKey -> do
                    pubKey <- pubKey |> updateRecord
                    setSuccessMessage "PubKey updated"
                    redirectTo EditPubKeyAction { .. }

    action CreatePubKeyAction = do
        let pubKey = newRecord @PubKey
        pubKey
            |> buildPubKey
            |> ifValid \case
                Left pubKey -> render NewView { .. } 
                Right pubKey -> do
                    pubKey <- pubKey |> createRecord
                    setSuccessMessage "PubKey created"
                    redirectTo PubKeysAction

    action DeletePubKeyAction { pubKeyId } = do
        pubKey <- fetch pubKeyId
        deleteRecord pubKey
        setSuccessMessage "PubKey deleted"
        redirectTo PubKeysAction

buildPubKey pubKey = pubKey
    |> fill @["pem","date"]
