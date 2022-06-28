module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data MessagesController
    = NewMessageAction
    | CreateMessageAction
    deriving (Eq, Show, Data)

data KeyController
    = KeysAction
    | NewKeyAction
    | ShowKeyAction { keyId :: !(Id Key) }
    | CreateKeyAction
    | EditKeyAction { keyId :: !(Id Key) }
    | UpdateKeyAction { keyId :: !(Id Key) }
    | DeleteKeyAction { keyId :: !(Id Key) }
    deriving (Eq, Show, Data)

data VerificationMessagesController
    = NewVerificationMessageAction
    | CreateVerificationMessageAction
    deriving (Eq, Show, Data)

data PubKeysController
    = PubKeysAction
    | NewPubKeyAction
    | ShowPubKeyAction { pubKeyId :: !(Id PubKey) }
    | CreatePubKeyAction
    | EditPubKeyAction { pubKeyId :: !(Id PubKey) }
    | UpdatePubKeyAction { pubKeyId :: !(Id PubKey) }
    | DeletePubKeyAction { pubKeyId :: !(Id PubKey) }
    deriving (Eq, Show, Data)
