{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Menu.Common
    ( menu
    ) where

import Miso (View (), class_, nav_)

import Model.Action (Action ())
import Utils (BemClass (), bemClass)


menu :: BemClass -> [Item] -> View Action
menu bemClass' items = nav_
    [ class_ $ bemClass "Menu" bemClass'
    ]
    items
type Item = View Action
