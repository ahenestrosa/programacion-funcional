module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

data IndexController = 
    IndexAction deriving (Eq, Show, Data)

data MessagesController
    = NewMessageAction
    | CreateMessageAction
    deriving (Eq, Show, Data)

data KeyController deriving (Eq, Show, Data)

data VerificationMessagesController
    = NewVerificationMessageAction
    | CreateVerificationMessageAction
    deriving (Eq, Show, Data)

data PubKeysController
    = PubKeysAction
    | ShowPubKeyAction { pubKeyId :: !(Id PubKey) }
    deriving (Eq, Show, Data)
