module Web.Controller.Index where

import Web.Controller.Prelude
import Web.View.Index.Show

instance Controller IndexController where
    action IndexAction { } = do
        render ShowView {}