{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Footer.Common
    ( footer
    ) where

import Miso (View (), button_, class_, div_, footer_, onClick, type_, value_)

import Model.Action (Action (ToggleMenuVisibility))
import Utils (BemClass (BemClass), bemClass)
import Views.Dumb.Link.Common (link)
import Views.Smart.Router.Utils (goMemorizing, goSets)


footer :: BemClass -> View Action
footer bemClass' = footer_
    [ class_ $ bemClass "Footer" bemClass'
    ]
    [ div_
        [ class_ . bemClass "Item" $ BemClass "Footer" [] []
        ]
        [ link (BemClass "Footer" [] []) goSets "Sets"
        ]
    , div_
        [ class_ . bemClass "Item" $ BemClass "Footer" [] []
        ]
        [ button_
            [ class_ . bemClass "Burger" $ BemClass "Footer" [] []
            , onClick ToggleMenuVisibility
            , type_ "button"
            , value_ "Menu"
            ]
            []
        ]
    , div_
        [ class_ . bemClass "Item" $ BemClass "Footer" [] []
        ]
        [ link (BemClass "Footer" [] []) goMemorizing "Memorizing"
        ]
    ]
