module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.PubKeys
import Web.Controller.VerificationMessages
import Web.Controller.Key
import Web.Controller.Messages
import Web.Controller.Static

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        -- Generator Marker
        , parseRoute @PubKeysController
        , parseRoute @VerificationMessagesController
        , parseRoute @KeyController
        , parseRoute @MessagesController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
