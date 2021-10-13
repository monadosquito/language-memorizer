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
import Miso.String (ms)

import Common (Set (Set), Unit ())
import Model.Action (Action (AddUnit))
import Model.Model (Model())
import Views.Smart.PageSwitcher.Common (pageSwitcher)

import qualified Miso as M

import qualified Model.Action as MA
import qualified Model.Model as MM
import qualified Utils as U


makeFieldsNoPrefix ''MM.EditedSet
makeFieldsNoPrefix ''MM.Model
makeFieldsNoPrefix ''MM.Pages
makeFieldsNoPrefix ''MM.Pagination
makeFieldsNoPrefix ''MM.Settings
makeFieldsNoPrefix ''Set
makeFieldsNoPrefix ''Unit

set :: U.BemClass -> U.SetIx -> Model -> M.View MA.Action
set bemClass' setIx' model = M.nodeHtmlKeyed "main" (M.Key "set")
    [ M.class_ $ U.bemClass "Set" bemClass'
    , M.onCreated $ MA.RefreshSet setIx'
    ]
    [ M.ul_
        [ M.class_ . U.bemClass "UnitFormContainer" $ U.BemClass "Set" [] [] 
        , M.id_ "editUnitFormsContainer"
        ]
        (map (\(unitIx, unit) -> unitForm
            (U.BemClass "Set" [ U.darkMode' model ] [])
            ( unitIx
            , model
                ^.. editedSet.ixedUnits.traversed.filtered
                    (\(unitIx', _) -> unitIx' == unitIx)
                ^? _head._2
                ^. non unit
            ))
        . U.paginate
            (model ^? pagination.units.ix setIx'.current ^. non (-1))
            (model ^. settings.unitsPageCount.to read)
        $ set' ^.. to U.set'.units._Just.traversed.withIndex)
    , pageSwitcher
        (U.BemClass "Set" [ U.darkMode' model ] [])
        (MA.Units setIx')
        (model ^? pagination.units.ix setIx'.count ^. non (-1))
        model
    , M.div_
        [ M.class_ . U.bemClass "Form"
            $ U.BemClass "Set" [ "inline", U.darkMode' model ] [ "bottom" ]
        ]
        ((either
            (const
                [ M.input_
                    [ M.class_ . U.bemClass "Button"
                        $ U.BemClass "Set" [ U.darkMode' model ] []
                    , M.onClick $ MA.ShareSet setIx'
                    , M.type_ "button"
                    , M.value_ "Share"
                    ]
                ])
            (const
                [ M.input_
                    [ M.class_ . U.bemClass "Button"
                        $ U.BemClass "Set" [ U.darkMode' model ] []
                    , M.onClick $ MA.UpdateSharedSet setIx'
                    , M.type_ "button"
                    , M.value_ "Update"
                    ]
                ])
            set')
        ++ [ M.input_
            [ M.class_ . U.bemClass "Button"
                $ U.BemClass
                    "Set"
                    [ U.darkMode' model
                    , if null (model ^. editedSet.ixedUnits)
                            && model ^. editedSet.name
                                == model ^. sets.ix setIx'.to U.set'.name.to ms
                        then "inactive"
                        else ""
                    ]
                    []
            , M.onClick $ MA.SaveSet
            , M.type_ "button"
            , M.value_ "Save"
            ]
        , M.input_
            [ M.class_ . U.bemClass "FormField" $ U.BemClass "Set" [] []
            , M.id_ "setNameInput"
            , M.onInput $ MA.EditSet MA.Name (-1)
            , M.value_ $ model ^. editedSet.name.to ms
            ]
        , M.input_
            [ M.class_ . U.bemClass "Button" $ U.BemClass "Set" [ U.darkMode' model ] []
            , M.onClick AddUnit
            , M.type_ "button"
            , M.value_ "+"
            ]
        ])
    ]
  where
    unitForm :: U.BemClass -> (U.UnitIx, Unit) -> M.View Action
    unitForm bemClass'' (unitIx, unit) = M.form_
        [ M.class_ $ U.bemClass "Form" bemClass''
        ]
        [ M.input_
            [ M.class_ . U.bemClass "FormField" $ U.BemClass "Form" [] []
            , M.name_ "_text"
            , M.onInput $ MA.EditSet MA.UnitText unitIx
            , M.value_ $ unit ^. text.to ms
            ]
        , M.ul_
            [ M.class_ . U.bemClass "FieldList" $ U.BemClass "Form" [] []
            ]
            . map
                (\(translateIx, translate) -> M.li_
                    [ M.class_ . U.bemClass "Translate" $ U.BemClass "Set" [] []
                    ]
                    [ M.input_
                        [ M.class_ . U.bemClass "FormField" $ U.BemClass "Form" [] []
                        , M.name_ "_translates+"
                        , M.onInput $ MA.EditSet (MA.UnitTranslate translateIx) unitIx
                        , M.value_ $ translate ^. to ms
                        ]
                    , M.input_
                        [ M.class_ . U.bemClass "Button"
                            $ U.BemClass "Form" [ U.darkMode' model ] [ "tangent" ]
                        , M.data_ "mark" "delete-translate"
                        , M.onClick $ MA.DeleteTranslate unitIx translateIx
                        , M.type_ "button"
                        , M.value_ "-"
                        ]
                    ])
            $ unit ^.. translates.traversed.withIndex
        , M.input_
            [ M.class_ . U.bemClass "Button"
                $ U.BemClass "Form" [ U.darkMode' model ] [ "under" ]
            , M.onClick $ MA.AddTranslate unitIx 
            , M.type_ "button"
            , M.value_ "+"
            ]
        , M.input_
            [ M.class_ . U.bemClass "Button" $ U.BemClass "Form" [ U.darkMode' model ] []
            , M.data_ "mark" "delete-unit"
            , M.onClick $ MA.DeleteUnit unitIx
            , M.type_ "button"
            , M.value_ "-"
            ]
        ]

    set' = model ^? sets.ix setIx' ^. non (Left $ Set "" Nothing)
