{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Providing.Root.Nontouchscreen
    ( root
    ) where

import Miso (View (), class_, div_, text)

import Model.Action (Action ())
import Model.Model (Model ())


root :: Model -> View Action
root _ = div_
    [ class_ "Root"
    ]
    [ text "Nontouchscreen"
    ]
