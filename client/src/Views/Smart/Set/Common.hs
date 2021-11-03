{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Set.Common
    ( set
    ) where

import Control.Lens ((^.), (^..), (^?))
import Control.Lens.TH (makeFieldsNoPrefix)
import Miso.String (ms)

import Common (Set (Set), Unit ())
import Model.Action (Action (AddUnit))
import Model.Model (Model())
import Views.Smart.PageSwitcher.Common (pageSwitcher)

import qualified Control.Lens.Combinators as CLC
import qualified Miso as M

import qualified Model.Action as MA
import qualified Model.Model as MM
import qualified Utils as U


makeFieldsNoPrefix ''MM.EditedSet
makeFieldsNoPrefix ''MM.LiteLanguageMemorizer
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
            (case (model ^. activeSetsType) of
                { MM.Local -> True; MM.MyShared -> True; _ -> False })
            ( unitIx
            , model
                ^.. editedSet.ixedUnits.CLC.traversed.CLC.filtered
                    (\(unitIx', _) -> unitIx' == unitIx)
                ^? CLC._head.CLC._2
                ^. CLC.non unit
            ))
        . U.paginate
            (model ^? pagination.units.CLC.ix setIx'.current ^. CLC.non (-1))
            (model ^. settings.unitsPageCount.CLC.to read)
        $ set' ^.. CLC.to U.set'.units.CLC._Just.CLC.traversed.CLC.withIndex)
    , pageSwitcher
        (U.BemClass "Set" [ U.darkMode' model ] [])
        (MA.Units setIx')
        (model ^? pagination.units.CLC.ix setIx'.count ^. CLC.non (-1))
        model
    , M.div_
        [ M.class_ . U.bemClass "Form"
            $ U.BemClass "Set" [ "inline", U.darkMode' model ] [ "bottom" ]
        ]
        (either
            (const
                [ M.input_
                    [ M.class_ . U.bemClass "Button"
                        $ U.BemClass "Set" [ U.darkMode' model ] []
                    , M.onClick $ MA.ShareSet setIx'
                    , M.type_ "button"
                    , M.value_ "Share"
                    ]
                , saveSetBtn
                , setNameElem True
                , M.input_
                    [ M.class_ . U.bemClass "Button"
                        $ U.BemClass "Set" [ U.darkMode' model ] []
                    , M.onClick AddUnit
                    , M.type_ "button"
                    , M.value_ "+"
                    ]
                ])
            (\case
                Left _         ->
                    [ M.input_
                        [ M.class_ . U.bemClass "Button"
                            $ U.BemClass "Set" [ U.darkMode' model ] []
                        , M.onClick $ MA.UpdateSharedSet setIx'
                        , M.type_ "button"
                        , M.value_ "Update"
                        ]
                    , saveSetBtn
                    , setNameElem True
                    ]
                Right (Left _) ->
                    [ M.input_
                        [ M.class_ . U.bemClass "Button"
                            $ U.BemClass "Set" [ U.darkMode' model ] []
                        , M.onClick $ MA.SetDownloadedSet
                        , M.type_ "button"
                        , M.value_ "Download"
                        ]
                    , setNameElem False
                    ]
                _              ->
                    [ setNameElem False
                    ])
            set')
    ]
  where
    setNameElem :: Editable -> M.View Action
    setNameElem False = M.span_
        [ M.class_ . U.bemClass "FormField" $ U.BemClass "Set" [] []
        ]
        [ M.text $ model ^. editedSet.name.CLC.to ms
        ]
    setNameElem True  = M.input_
        [ M.class_ . U.bemClass "FormField" $ U.BemClass "Set" [] []
        , M.id_ "setNameInput"
        , M.onInput $ MA.EditSet MA.Name (-1)
        , M.value_ $ model ^. editedSet.name.CLC.to ms
        ]

    unitForm :: U.BemClass -> Editable -> (U.UnitIx, Unit) -> M.View Action
    unitForm bemClass'' True (unitIx, unit) = M.form_
        [ M.class_ $ U.bemClass "Form" bemClass''
        ]
        [ M.input_
            [ M.class_ . U.bemClass "FormField" $ U.BemClass "Form" [] []
            , M.name_ "_text"
            , M.onInput $ MA.EditSet MA.UnitText unitIx
            , M.value_ $ unit ^. text.CLC.to ms
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
                        , M.value_ $ translate ^. CLC.to ms
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
            $ unit ^.. translates.CLC.traversed.CLC.withIndex
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
    unitForm bemClass'' False (_, unit) = M.div_
        [ M.class_ $ U.bemClass "Form" bemClass''
        ]
        [ M.span_
            [ M.class_ . U.bemClass "FormField" $ U.BemClass "Form" [] []
            ]
            [ M.text $ unit ^. text.CLC.to ms
            ]
        , M.ul_
            [ M.class_ . U.bemClass "FieldList" $ U.BemClass "Form" [] []
            ]
            . map
                (\translate -> M.li_ []
                    [ M.div_
                        [ M.class_ . U.bemClass "FormField" $ U.BemClass "Form" [] []
                        ]
                        [ M.text $ translate ^. CLC.to ms
                        ]
                    ])
            $ unit ^. translates
        ]

    saveSetBtn = M.input_
        [ M.class_ . U.bemClass "Button"
            $ U.BemClass
                "Set"
                [ U.darkMode' model
                , if null (model ^. editedSet.ixedUnits)
                        && model ^. editedSet.name
                            == model
                                ^. sets.CLC.ix setIx'.CLC.to
                                    U.set'.name.CLC.to ms
                    then "inactive"
                    else ""
                ]
                []
        , M.onClick $ MA.SaveSet
        , M.type_ "button"
        , M.value_ "Save"
        ]
    set' = model ^? sets.CLC.ix setIx' ^. CLC.non (Left $ Set "" Nothing)
type Editable = Bool
