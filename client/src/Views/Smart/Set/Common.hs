{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Set.Common
    ( set
    ) where

import Control.Lens ((^.), (^..), (^?))
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Combinators (_2, _Just, _head, filtered, ix, non, to, traversed, withIndex)

import Model.Action (Action (AddUnit))
import Model.Model (Model (), Set (Set), Unit ())
import Utils (BemClass (BemClass), SetIx (), UnitIx (), bemClass, darkMode', paginate)
import Views.Smart.PageSwitcher.Common (pageSwitcher)

import qualified Miso as M

import qualified Model.Action as MA
import qualified Model.Model as MM


makeFieldsNoPrefix ''MM.EditedSet
makeFieldsNoPrefix ''MM.Model
makeFieldsNoPrefix ''MM.Pages
makeFieldsNoPrefix ''MM.Pagination
makeFieldsNoPrefix ''MM.Set
makeFieldsNoPrefix ''MM.Settings
makeFieldsNoPrefix ''MM.Unit

set :: BemClass -> SetIx -> Model -> M.View MA.Action
set bemClass' setIx' model = M.nodeHtmlKeyed "main" (M.Key "set")
    [ M.class_ $ bemClass "Set" bemClass'
    , M.onCreated $ MA.RefreshSet setIx'
    ]
    [ M.ul_
        [ M.class_ . bemClass "UnitFormContainer" $ BemClass "Set" [] [] 
        , M.id_ "editUnitFormsContainer"
        ]
        (map (\(unitIx, unit) -> unitForm
            (BemClass "Set" [ darkMode' model ] [])
            ( unitIx
            , model
                ^.. editedSet.ixedUnits.traversed.filtered
                    (\(unitIx', _) -> unitIx' == unitIx)
                ^? _head._2
                ^. non unit
            ))
        . paginate
            (model ^? pagination.units.ix setIx'.current ^. non (-1))
            (model ^. settings.unitsPageCount.to read)
        $ set' ^.. units._Just.traversed.withIndex)
    , pageSwitcher
        (BemClass "Set" [ darkMode' model ] [])
        (MA.Units setIx')
        (model ^? pagination.units.ix setIx'.count ^. non (-1))
        model
    , M.div_
        [ M.class_ . bemClass "Form"
            $ BemClass "Set" [ "inline", darkMode' model ] [ "bottom" ]
        ]
        [ M.input_
            [ M.class_ . bemClass "Button" $ BemClass "Form" [ darkMode' model ] []
            , M.onClick $ MA.ShareSet setIx'
            , M.type_ "button"
            , M.value_ "Share"
            ]
        , M.input_
            [ M.class_ . bemClass "Button"
                $ BemClass
                    "Set"
                    [ darkMode' model
                    , if null (model ^. editedSet.ixedUnits)
                            && model ^. editedSet.name == model ^. sets.ix setIx'.name
                        then "inactive"
                        else ""
                    ]
                    []
            , M.onClick $ MA.SaveSet
            , M.type_ "button"
            , M.value_ "Save"
            ]
        , M.input_
            [ M.class_ . bemClass "FormField" $ BemClass "Set" [] []
            , M.id_ "setNameInput"
            , M.onInput $ MA.EditSet MA.Name (-1)
            , M.value_ $ model ^. editedSet.name
            ]
        , M.input_
            [ M.class_ . bemClass "Button" $ BemClass "Set" [ darkMode' model ] []
            , M.onClick AddUnit
            , M.type_ "button"
            , M.value_ "+"
            ]
        ]
    ]
  where
    unitForm :: BemClass -> (UnitIx, Unit) -> M.View Action
    unitForm bemClass'' (unitIx, unit) = M.form_
        [ M.class_ $ bemClass "Form" bemClass''
        ]
        [ M.input_
            [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
            , M.name_ "_text"
            , M.onInput $ MA.EditSet MA.UnitText unitIx
            , M.value_ $ unit ^. text
            ]
        , M.ul_
            [ M.class_ . bemClass "FieldList" $ BemClass "Form" [] []
            ]
            . map
                (\(translateIx, translate) -> M.li_
                    [ M.class_ . bemClass "Translate" $ BemClass "Set" [] []
                    ]
                    [ M.input_
                        [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                        , M.name_ "_translates+"
                        , M.onInput $ MA.EditSet (MA.UnitTranslate translateIx) unitIx
                        , M.value_ translate
                        ]
                    , M.input_
                        [ M.class_ . bemClass "Button"
                            $ BemClass "Form" [ darkMode' model ] [ "tangent" ]
                        , M.data_ "mark" "delete-translate"
                        , M.onClick $ MA.DeleteTranslate unitIx translateIx
                        , M.type_ "button"
                        , M.value_ "-"
                        ]
                    ])
            $ unit ^.. translates.traversed.withIndex
        , M.input_
            [ M.class_ . bemClass "Button"
                $ BemClass "Form" [ darkMode' model ] [ "under" ]
            , M.onClick $ MA.AddTranslate unitIx 
            , M.type_ "button"
            , M.value_ "+"
            ]
        , M.input_
            [ M.class_ . bemClass "Button" $ BemClass "Form" [ darkMode' model ] []
            , M.data_ "mark" "delete-unit"
            , M.onClick $ MA.DeleteUnit unitIx
            , M.type_ "button"
            , M.value_ "-"
            ]
        ]

    set' = model ^? sets.ix setIx' ^. non (Set "" Nothing)
