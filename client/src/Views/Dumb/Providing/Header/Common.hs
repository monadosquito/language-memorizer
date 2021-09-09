{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Providing.Header.Common
    ( header
    ) where

import Miso (View (), class_, header_, nav_)

import Model.Action (Action ())
import Model.Model (Model ())
import Utils (BemClass (BemClass), bemClass, darkMode')
import Views.Dumb.Link.Common (link)
import Views.Smart.Router.Utils (goHome, goMemorizing, goSets, goSettings, goStatistics)


header :: BemClass -> Model -> View Action
header bemClass' model = header_
    [ class_ $ bemClass "Header" bemClass'
    ]
    [ nav_
        [ class_ . bemClass "LinkList" $ BemClass "Header" [] []
        ]
        [ link (BemClass "Header" [] []) goHome "Home"
        , link (BemClass "Menu" [ darkMode' model ] []) goMemorizing "Memorizing"
        , link (BemClass "Menu" [ darkMode' model ] []) goStatistics "Statistics"
        , link (BemClass "Menu" [ darkMode' model ] []) goSettings "Settings"
        , link (BemClass "Menu" [ darkMode' model ] []) goSets "Sets"
        ]
    ]

