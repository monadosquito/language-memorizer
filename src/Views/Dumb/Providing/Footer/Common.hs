{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Providing.Footer.Common
    ( footer
    ) where

import Miso (View (), button_, class_, div_, footer_, onClick, type_, value_)

import Model.Action (Action (ToggleMenuVisibility))
import Model.Model (Model ())
import Utils (BemClass (BemClass), bemClass, darkMode')
import Views.Dumb.Link.Common (link)
import Views.Smart.Router.Utils (goMemorizing, goStatistics)


footer :: BemClass -> Model -> View Action
footer bemClass' model = footer_
    [ class_ $ bemClass "Footer" bemClass'
    ]
    [ div_
        [ class_ . bemClass "Item" $ BemClass "Footer" [] []
        ]
        [ link (BemClass "Footer" [] []) goStatistics "Statistics"
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
        [ link (BemClass "Footer" [ darkMode' model ] []) goMemorizing "Memorizing"
        ]
    ]
