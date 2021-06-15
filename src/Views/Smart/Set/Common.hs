{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Set.Common
    ( set
    ) where

import Control.Lens ((^.), (^?))
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Combinators (ix, non)

import Model.Action (Action (AddUnit))
import Model.Model (Model(), Set (Set), Unit ())
import Utils (BemClass (BemClass), FormMark (), SetIx (), bemClass)

import qualified Miso as M

import qualified Model.Action as A


makeFieldsNoPrefix ''Model
makeFieldsNoPrefix ''Set
makeFieldsNoPrefix ''Unit

set :: BemClass -> SetIx -> Model -> M.View A.Action
set bemClass' setIx model = M.nodeHtmlKeyed "main" (M.Key "set")
    [ M.class_ $ bemClass "Set" bemClass'
    , M.onCreated $ A.RefreshSet setIx
    ]
    [ M.div_
        [ M.id_ "editSetPseudoForm"
        ]
        [ M.ul_
            [ M.class_ . bemClass "UnitFormContainer" $ BemClass "Set" [] [] 
            , M.id_ "editUnitFormsContainer"
            ]
            (map
                (\unit -> M.li_ []
                    [ unitForm (BemClass "Set" [] []) "edit-unit-form" setIx unit
                    ])
                $ set' ^. units.non [])
        ]
    , M.div_
        [ M.class_ . bemClass "Form" $ BemClass "Set" [ "inline" ] []
        ]
        [ M.input_
            [ M.class_ . bemClass "Button" $ BemClass "Set" [ "hidden" ] []
            , M.id_ "saveSetButton"
            , M.onClick $ A.SaveSet setIx
            , M.type_ "button"
            , M.value_ "Save"
            ]
        , M.input_
            [ M.class_ . bemClass "FormField" $ BemClass "Set" [] []
            , M.id_ "setNameInput"
            , M.value_ $ set' ^. name
            ]
        , M.input_
            [ M.class_ . bemClass "Button" $ BemClass "Set" [] [ "hidden" ]
            , M.onClick AddUnit
            , M.type_ "button"
            , M.value_ "+"
            ]
        ]
    ]
  where
    unitForm :: BemClass -> FormMark -> SetIx -> Unit -> M.View Action
    unitForm bemClass'' formMark _ unit = M.form_
        [ M.class_ $ bemClass "Form" bemClass''
        , M.data_ "mark" formMark
        ]
        [ M.input_
            [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
            , M.name_ "_text"
            , M.value_ $ unit ^. text
            ]
        , M.ul_
            [ M.class_ . bemClass "FieldList" $ BemClass "Form" [] []
            ]
            . map
                (\translate -> M.li_
                    [ M.class_ . bemClass "Translate" $ BemClass "Set" [] []
                    ]
                    [ M.input_
                        [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                        , M.name_ "_translates+"
                        , M.value_ translate
                        ]
                    , M.input_
                        [ M.class_ . bemClass "Button" $ BemClass "Form" [] [ "tangent" ]
                        , M.data_ "mark" "delete-translate"
                        , M.type_ "button"
                        , M.value_ "-"
                        ]
                    ])
            $ unit ^. translates
        , M.input_
            [ M.class_ . bemClass "Button" $ BemClass "Form" [] [ "under" ]
            , M.data_ "mark" "add-translate"
            , M.type_ "button"
            , M.value_ "+"
            ]
        , M.input_
            [ M.class_ . bemClass "Button" $ BemClass "Form" [] []
            , M.data_ "mark" "delete-unit"
            , M.type_ "button"
            , M.value_ "-"
            ]
        ]

    set' = model ^? sets.ix setIx ^. non (Set "" Nothing)
