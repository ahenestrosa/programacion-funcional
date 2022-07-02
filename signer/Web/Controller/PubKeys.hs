module Web.Controller.PubKeys where

import Web.Controller.Prelude
import Web.View.PubKeys.Index
import Web.View.PubKeys.Show

instance Controller PubKeysController where
    action PubKeysAction = do
        pubKeys <- query @PubKey |> fetch
        render IndexView { .. }

    action ShowPubKeyAction { pubKeyId } = do
        pubKey <- fetch pubKeyId
        render ShowView { .. }

