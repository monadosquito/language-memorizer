{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Providing.Header.Common
    ( header
    ) where

import Miso (View (), class_, header_, nav_)

import Model.Action (Action ())
import Model.Model (Model ())
import Utils (BemClass (BemClass), bemClass, darkMode')
import Views.Dumb.Link.Common (link)
import Views.Smart.Avatar.Common (avatar)

import qualified Views.Smart.Router.Utils as VSMRU


header :: BemClass -> Model -> View Action
header bemClass' model = header_
    [ class_ $ bemClass "Header" bemClass'
    ]
    [ nav_
        [ class_ . bemClass "LinkList" $ BemClass "Header" [] []
        ]
        [ link (BemClass "Header" [] []) VSMRU.goHome "Home"
        , link (BemClass "Menu" [ darkMode' model ] []) VSMRU.goMemorizing "Memorizing"
        , link (BemClass "Menu" [ darkMode' model ] []) VSMRU.goStatistics "Statistics"
        , link (BemClass "Menu" [ darkMode' model ] []) VSMRU.goSettings "Settings"
        , link (BemClass "Menu" [ darkMode' model ] []) VSMRU.goSets "Sets"
        , avatar (BemClass "Menu" [ darkMode' model ] []) model
        , link (BemClass "Menu" [ darkMode' model ] []) VSMRU.goSignIn "SignIn"
        , link (BemClass "Menu" [ darkMode' model ] []) VSMRU.goSignUp "SignUp"
        ]
    ]

