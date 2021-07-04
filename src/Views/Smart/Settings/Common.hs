{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Settings.Common
    ( settings'
    ) where

import Control.Lens ((^.))
import Control.Lens.Combinators (to)
import Control.Lens.TH (makeFieldsNoPrefix)
import Miso.String (ms)

import Model.Action (Action (SaveSettings))
import Model.Model (Model (), Settings ())
import Utils (BemClass (BemClass), bemClass)

import qualified Miso as M


makeFieldsNoPrefix ''Model
makeFieldsNoPrefix ''Settings

settings' :: BemClass -> Model -> M.View Action
settings' bemClass' model = M.main_
    [ M.class_ $ bemClass "Settings" bemClass'
    ]
    [ M.form_
        [ M.class_ . bemClass "Form" $ BemClass "Settings" [] []
        , M.data_ "mark" "edit-settings-form"
        , M.onSubmit SaveSettings
        ]
        [ M.label_ []
            [ M.input_
                [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                , M.name_ "_setsPageCount"
                , M.value_ $ model ^. settings.setsPageCount.to ms
                ]
            , M.text "Sets page count"
            ]
        , M.label_ []
            [ M.input_
                [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                , M.name_ "_unitsPageCount"
                , M.value_ $ model ^. settings.unitsPageCount.to ms
                ]
            , M.text "Units page count"
            ]
        , M.input_
            [ M.class_ . bemClass "Button" $ BemClass "Settings" [] []
            , M.type_ "submit"
            , M.value_ "Save"
            ]
        ]
    ]
