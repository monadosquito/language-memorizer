{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.ActivePage.Common
    ( activePage
    ) where

import Miso (View(), class_, section_)

import Model.Action (Action ())
import Utils (BemClass (), bemClass)


activePage :: BemClass -> Page -> View Action
activePage bemClass' page = section_
    [ class_ $ bemClass "ActivePage" bemClass'
    ]
    [ page
    ]
type Page = View Action
