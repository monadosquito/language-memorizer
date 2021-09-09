{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Dumb.Providing.Root.Touchscreen
    ( root
    ) where

import Control.Lens ((^.))
import Control.Lens.TH (makeFieldsNoPrefix)
import Miso (View (), class_, div_)

import Model.Action (Action ())
import Model.Model (Model ())
import Utils (BemClass (BemClass), darkMode')
import Views.Dumb.ActivePage.Common (activePage)
import Views.Dumb.Providing.Footer.Common (footer)
import Views.Dumb.Link.Common (link)
import Views.Dumb.Menu.Common (menu)
import Views.Smart.Router.Common (router)
import Views.Smart.Router.Utils (goHome, goSets, goSettings)


makeFieldsNoPrefix ''Model

root :: Model -> View Action
root model = div_
    [ class_ "Root"
    ]
    [ activePage (BemClass "Root" [ darkMode' model ] []) $ router model
    , menu
        (BemClass "Root" [ darkMode' model ]
            $ if model ^. menuIsVisible then [ "visible" ] else [])
        [ link (BemClass "Root" [ darkMode' model ] []) goHome "Home"
        , link (BemClass "Root" [ darkMode' model ] []) goSettings "Settings"
        , link (BemClass "Root" [ darkMode' model ] []) goSets "Sets"
        ]
    , footer (BemClass "Root" [ darkMode' model ] []) model
    ]
