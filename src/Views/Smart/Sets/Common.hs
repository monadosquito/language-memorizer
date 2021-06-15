{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Sets.Common
    ( sets'
    ) where

import Control.Lens ((^.), to)
import Control.Lens.TH (makeFieldsNoPrefix)

import Model.Action (Action (AddSet, DeleteSet))
import Model.Model (Model (), Set ())
import Utils (BemClass (BemClass), bemClass)
import Views.Smart.Router.Utils (goSet)

import qualified Miso as M


makeFieldsNoPrefix ''Model
makeFieldsNoPrefix ''Set

sets' :: BemClass -> Model -> M.View Action
sets' bemClass' model = M.main_
    [ M.class_ $ bemClass "Sets" bemClass'
    ]
    [ M.ul_
        [ M.class_ . bemClass "SetList" $ BemClass "Sets" [] []
        ] (zipWith (\set setIx -> M.li_
            [ M.class_ . bemClass "SetListItem" $ BemClass "Sets" [] []
            ]
            [ M.a_
                [ M.class_ . bemClass "SetListItemName" $ BemClass "Sets" [] []
                , M.onClick $ goSet setIx
                ]
                [ M.text $ set ^. name
                ]
            , M.input_
                [ M.class_ . bemClass "Button" $ BemClass "Sets" [] []
                , M.onClick $ DeleteSet setIx
                , M.type_ "button"
                , M.value_ "-"
                ]
            ])
            (model ^. sets)
            [ 0..model ^. sets.to (subtract 1 . length) ])
    , M.form_
        [ M.class_ . bemClass "Form" $ BemClass "Sets" [ "inline" ] [ "inline" ]
        , M.data_ "mark" "add-set-form"
        , M.onSubmit AddSet
        ]
        [ M.input_
            [ M.class_ . bemClass "FormField" $ BemClass "Sets" [] []
            , M.name_ "_name"
            , M.placeholder_ "Set name"
            ]
        , M.input_
            [ M.class_ . bemClass "Button" $ BemClass "Sets" [] []
            , M.type_ "submit"
            , M.value_ "+"
            ]
        ]
    ]
