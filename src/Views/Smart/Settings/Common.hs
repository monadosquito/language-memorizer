{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Settings.Common
    ( settings'
    ) where

import Control.Lens ((^.), (^..), (^?))
import Control.Lens.Combinators (_Just, non, to, traversed, withIndex)
import Control.Lens.TH (makeFieldsNoPrefix)
import Miso.String (ms)

import Model.Action (Action (SaveSettings))
import Model.Model (MemorizingMode (..), Model (), Set (), Settings ())
import Utils (BemClass (BemClass), bemClass)

import qualified Miso as M


makeFieldsNoPrefix ''Model
makeFieldsNoPrefix ''Set
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
            [ M.select_
                [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                , M.multiple_ True
                , M.name_ "_activeSetIxs+"
                ]
                . map (\(setIx, set) -> M.option_
                    [ M.name_ "_activeSetIxs+"
                    , M.selected_
                        $ model
                            ^? settings.activeSetIxs._Just
                            ^. non [].to (elem setIx . map read)
                    , M.value_ $ ms setIx
                    ]
                    [ M.span_ []
                        [ M.text $ set ^. name
                        ]
                    ]
                )
                $ model ^.. sets.traversed.withIndex
            , M.text "Active sets"
            ]
        , M.label_ []
            [ M.select_
                [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                , M.name_ "_memorizingMode"
                ]
                $ case model ^. settings.memorizingMode of
                    Text       ->
                        [ M.option_
                            [ M.selected_ True
                            , M.value_ "Text"
                            ]
                            [ M.text "Text"
                            ]
                        , M.option_
                            [ M.value_ "Translates"
                            ]
                            [ M.text "Translates"
                            ]
                        ]
                    Translates ->
                        [ M.option_
                            [ M.value_ "Text"
                            ]
                            [ M.text "Text"
                            ]
                        , M.option_
                            [ M.selected_ True
                            , M.value_ "Translates"
                            ]
                            [ M.text "Translates"
                            ]
                        ]
            , M.text "Memorizing mode"
            ]
        , M.label_ []
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
                , M.name_ "_statisticsPageCount"
                , M.value_ $ model ^. settings.statisticsPageCount.to ms
                ]
            , M.text "Statistics page count"
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
