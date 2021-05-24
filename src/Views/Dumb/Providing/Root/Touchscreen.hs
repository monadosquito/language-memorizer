{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Providing.Root.Touchscreen
    ( root
    ) where

import Miso (View (), class_, div_)

import Model.Action (Action ())
import Model.Model (Model ())
import Utils (BemClass (BemClass))
import Views.Dumb.Home.Common (home)


root :: Model -> View Action
root _ = div_
    [ class_ "Root"
    ]
    [ home $ BemClass "Root" [] []
    ]
