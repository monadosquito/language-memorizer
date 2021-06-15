{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Providing.Root.Nontouchscreen
    ( root
    ) where

import Miso (View (), class_, div_)

import Model.Action (Action ())
import Model.Model (Model ())
import Utils (BemClass (BemClass))
import Views.Dumb.ActivePage.Common (activePage)
import Views.Dumb.Header.Common (header)
import Views.Smart.Router.Common (router)


root :: Model -> View Action
root model = div_
    [ class_ "Root"
    ]
    [ activePage (BemClass "Root" [] []) $ router model
    , header $ BemClass "Root" [] []
    ]
