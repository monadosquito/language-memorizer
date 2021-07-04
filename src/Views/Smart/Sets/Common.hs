{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Sets.Common
    ( sets'
    ) where

import Control.Lens ((^.), to)
import Control.Lens.TH (makeFieldsNoPrefix)

import Model.Action (Action (AddSet, DeleteSet), Paginated (Sets))
import Model.Model (Model (), Pages (), Pagination (), Set (), Settings ())
import Utils (BemClass (BemClass), bemClass, paginate)
import Views.Dumb.PageSwitcher.Common (pageSwitcher)
import Views.Smart.Router.Utils (goSet)

import qualified Miso as M


makeFieldsNoPrefix ''Model
makeFieldsNoPrefix ''Pages
makeFieldsNoPrefix ''Pagination
makeFieldsNoPrefix ''Set
makeFieldsNoPrefix ''Settings

sets' :: BemClass -> Model -> M.View Action
sets' bemClass' model = M.main_
    [ M.class_ $ bemClass "Sets" bemClass'
    ]
    [ M.ul_
        [ M.class_ $ bemClass "SetList" $ BemClass "Sets" [] []
        ]
        . map (\(setIx, set') -> M.li_ 
            [ M.class_ . bemClass "SetListItem" $ BemClass "Sets" [] []
            ]
            [ M.a_
                [ M.class_ . bemClass "SetListItemName" $ BemClass "Sets" [] []
                , M.onClick $ goSet setIx
                ]
                [ M.text $ set' ^. name
                ]
            , M.input_
                [ M.class_ . bemClass "Button" $ BemClass "Sets" [] []
                , M.onClick $ DeleteSet setIx
                , M.type_ "button"
                , M.value_ "-"
                ]
            ])
        . paginate
            (model ^. pagination.sets.current)
            (model ^. settings.setsPageCount.to read)
        . zip [ 0..model ^. sets.to (subtract 1 . length) ]
        $ model ^. sets
    , pageSwitcher (BemClass "Sets" [] []) Sets $ model ^. pagination.sets.count
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
