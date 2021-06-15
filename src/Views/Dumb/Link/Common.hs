{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Link.Common
    ( link
    ) where

import Miso (View (), a_, class_, onClick, text)
import Miso.String (MisoString ())

import Model.Action (Action ())
import Utils (BemClass (), bemClass)


link :: BemClass -> Action -> Label -> View Action
link bemClass' action label = a_
    [ class_ $ bemClass "Link" bemClass'
    , onClick action
    ]
    [ text label
    ]
type Label = MisoString
