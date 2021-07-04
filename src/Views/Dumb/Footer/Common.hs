{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Footer.Common
    ( footer
    ) where

import Miso (View (), class_, footer_, nav_)

import Model.Action (Action ())
import Utils (BemClass (BemClass), bemClass)
import Views.Dumb.Link.Common (link)
import Views.Smart.Router.Utils (goHome, goSets, goSettings)


footer :: BemClass -> View Action
footer bemClass' = footer_
    [ class_ $ bemClass "Footer" bemClass'
    ]
    [ nav_
        [ class_ . bemClass "LinkList" $ BemClass "Footer" [] []
        ]
        [ link (BemClass "Footer" [] []) goHome "Home"
        , link (BemClass "Footer" [] []) goSettings "Settings"
        , link (BemClass "Footer" [] []) goSets "Sets"
        ]
    ]
