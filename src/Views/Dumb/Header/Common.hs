{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Header.Common
    ( header
    ) where

import Miso (View (), class_, header_, nav_)

import Model.Action (Action ())
import Utils (BemClass (BemClass), bemClass)
import Views.Dumb.Link.Common (link)
import Views.Smart.Router.Utils (goHome, goSets)


header :: BemClass -> View Action
header bemClass' = header_
    [ class_ $ bemClass "Header" bemClass'
    ]
    [ nav_
        [ class_ . bemClass "LinkList" $ BemClass "Header" [] []
        ]
        [ link (BemClass "Header" [] []) goHome "Home"
        , link (BemClass "Header" [] []) goSets "Sets"
        ]
    ]

