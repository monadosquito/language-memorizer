{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Dumb.Providing.Root.Nontouchscreen
    ( root
    ) where

import Control.Lens.TH (makeFieldsNoPrefix)
import Miso (View (), class_, div_)

import Model.Action (Action ())
import Model.Model (Model (), Settings ())
import Utils (BemClass (BemClass), darkMode')
import Views.Dumb.ActivePage.Common (activePage)
import Views.Dumb.Providing.Header.Common (header)
import Views.Smart.Router.Common (router)


makeFieldsNoPrefix ''Model
makeFieldsNoPrefix ''Settings

root :: Model -> View Action
root model = div_
    [ class_ "Root"
    ]
    [ activePage (BemClass "Root" [ darkMode' model ] []) $ router model
    , header (BemClass "Root" [ darkMode' model ] []) model
    ]
