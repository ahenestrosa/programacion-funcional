module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data MessagesController
    = MessagesAction
    | NewMessageAction
    | ShowMessageAction { messageId :: !(Id Message) }
    | CreateMessageAction
    | EditMessageAction { messageId :: !(Id Message) }
    | UpdateMessageAction { messageId :: !(Id Message) }
    | DeleteMessageAction { messageId :: !(Id Message) }
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
