{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Memorizing.Common
    ( memorizing'
    ) where

import Control.Lens ((^.), (^?))
import Control.Lens.Combinators (_Just, ix, non, to)
import Control.Lens.TH (makeFieldsNoPrefix)
import Miso.String (ms)

import Utils (BemClass (BemClass), ElementModifier (), bemClass)

import qualified Miso as M

import qualified Model.Model as MM
import qualified Model.Action as MA


makeFieldsNoPrefix ''MM.LiteSet
makeFieldsNoPrefix ''MM.Memorizing
makeFieldsNoPrefix ''MM.Model
makeFieldsNoPrefix ''MM.Set
makeFieldsNoPrefix ''MM.Settings
makeFieldsNoPrefix ''MM.Unit

memorizing' :: BemClass -> MM.Model -> M.View MA.Action
memorizing' bemClass' model = M.main_
    [ M.class_ $ bemClass "Memorizing" bemClass'
    ]
    (if model ^. memorizing.initLiteSetsLen == 0
    then
        [ M.h1_ []
            [ M.text "Add Set(s) and/or Unit(s) or set Set(s) as Active Set(s) via Settings."
            ]
        ]
    else
        ([ M.div_
            [ M.class_ . bemClass "Bar" $ BemClass "Memorizing" [] []
            ]
            ((map
                (\progressStep ->
                    if progressStep
                    then progressStepElem "succeeded"
                    else progressStepElem "failed")
                $ memorizing'' ^. progress)
            ++ (replicate
                (memorizing'' ^. initLiteSetsLen - memorizing'' ^. progress.to length)
                $ M.span_
                    [ M.class_ . bemClass "Segment" $ BemClass "Bar" [] []
                    ]
                    []))
        ]
        ++ (case maybeUnit of
            Nothing    -> []
            Just unit' ->
                ((case memorizingMode' of
                    MM.Text       -> textMemorizing unit'
                    MM.Translates -> tranlatesMemorizing unit')))
        ++ [ M.div_ 
            [ M.class_ . bemClass "ControlPanel" $ BemClass "Memorizing" [] []
            ]
            ([ M.input_
                [ M.class_ . bemClass "Button" $ BemClass "Memorizing" [] []
                , M.onClick MA.RepeatMemorizing
                , M.type_ "button"
                , M.value_ "Repeat"
                ]
            ]
            ++ case memorizingMode' of
                MM.Text       -> [ showBtn $ MA.ShowAnswer MA.Text' ]
                MM.Translates -> [ showBtn $ MA.ShowAnswer MA.Translates ]
            ++ (if memorizing'' ^. pause
            then [ continueBtn False memorizing'' model ]
            else [ continueBtn True memorizing'' model ]))
        ]))
  where
    continueBtn :: Inactive -> MM.Memorizing -> MM.Model -> M.View MA.Action
    continueBtn inactive memorizing''' model' = M.input_
        [ M.class_ . bemClass "Button"
            $ BemClass "Memorizing"
                [ if inactive then "inactive" else "" ]
                []
        , M.onClick
            $ if inactive
            then MA.DoNothing
            else MA.CheckAnswer
                (case memorizingMode' of
                    MM.Translates -> MA.Translates
                    MM.Text       -> MA.Text')
                $ model'
                    ^? sets.ix (memorizing''' ^. setIx)
                        .units._Just.ix (memorizing''' ^. unitIx).text
                    ^. non ""
        , M.type_ "button"
        , M.value_ "Continue"
        ]

    progressStepElem :: ElementModifier -> M.View MA.Action
    progressStepElem status = M.span_
        [ M.class_ . bemClass "Segment"
            $ BemClass "Bar" [] [ status ]
        ]
        []

    showBtn :: MA.Action -> M.View MA.Action
    showBtn act = M.input_
        [ M.class_ . bemClass "Button" $ BemClass "Memorizing" [] []
        , M.onClick act
        , M.type_ "button"
        , M.value_ "Show"
        ]

    textMemorizing :: MM.Unit -> [M.View MA.Action]
    textMemorizing unit' =
        [ M.span_
            [ M.class_ . bemClass "Text" $ BemClass "Memorizing" [] []
            ]
            [ M.text
                $ unit'
                    ^? translates.ix (memorizing'' ^. translateIx)
                    ^. non "".to ms
            ]
        , M.form_
            [ M.class_ . bemClass "Form" $ BemClass "Memorizing" [] []
            ]
            [ M.input_
                [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                , M.onInput $ MA.CheckAnswer MA.Text'
                , M.value_ $ memorizing'' ^. answer
                ]
            ]
        ]

    tranlatesMemorizing :: MM.Unit -> [M.View MA.Action]
    tranlatesMemorizing unit' =
        [ M.span_
            [ M.class_ . bemClass "Text" $ BemClass "Memorizing" [] []
            ]
            [ M.text $ unit' ^. text
            ]
        , M.form_ []
            [ M.input_
                [ M.onInput $ MA.CheckAnswer MA.Translates
                ]
            ]
        , M.input_
            [ M.onClick $ MA.ShowAnswer MA.Translates
            , M.type_ "button"
            , M.value_ "Show"
            ]
        ]

    maybeUnit       = model
        ^? sets.ix (memorizing'' ^. setIx).units._Just.ix (memorizing'' ^. unitIx)
    memorizing''    = model ^. memorizing
    memorizingMode' = model ^. settings.memorizingMode
type Inactive = Bool
