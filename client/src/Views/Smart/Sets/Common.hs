{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Sets.Common
    ( sets'
    ) where

import Control.Lens ((^.), (^..), to)
import Control.Lens.Combinators (_Left, _Right, withIndex)
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Traversal (traversed)
import Miso.String (ms)

import Common (LiteSharedSet (), Set (), SharedSet ())
import Utils (BemClass (BemClass), SetIx (), bemClass, darkMode', paginate)
import Views.Smart.PageSwitcher.Common (pageSwitcher)
import Views.Smart.Router.Utils (goSet)

import qualified Miso as M

import qualified Model.Action as MA
import qualified Model.Model as MM


makeFieldsNoPrefix ''LiteSharedSet
makeFieldsNoPrefix ''MM.DownloadedSet
makeFieldsNoPrefix ''MM.Model
makeFieldsNoPrefix ''MM.Pages
makeFieldsNoPrefix ''MM.Pagination
makeFieldsNoPrefix ''MM.SetsPagination
makeFieldsNoPrefix ''MM.Settings
makeFieldsNoPrefix ''Set
makeFieldsNoPrefix ''SharedSet

sets' :: BemClass -> MM.Model -> M.View MA.Action
sets' bemClass' model = M.main_
    [ M.class_ $ bemClass "Sets" bemClass'
    ]
    ([ M.ul_
        [ M.class_ . bemClass "TabList" $ BemClass "Sets" [] [ darkMode' model ]
        ]
        [ M.input_
            [ M.class_ . bemClass "Tab"
                $ BemClass
                    "Sets"
                    []
                    [ darkMode' model
                    , if activeSetsType' == MM.Downloaded then "active" else ""
                    ]
            , M.onClick $ MA.SwitchActiveSetsType MA.Downloaded
            , M.type_ "button"
            , M.value_ "Downloaded"
            ]
        , M.input_
            [ M.class_ . bemClass "Tab"
                $ BemClass
                    "Sets"
                    []
                    [ darkMode' model
                    , if activeSetsType' == MM.Local then "active" else ""
                    ]
            , M.onClick $ MA.SwitchActiveSetsType MA.MyLocal
            , M.type_ "button"
            , M.value_ "Local"
            ]
        , M.input_
            [ M.class_ . bemClass "Tab"
                $ BemClass
                    "Sets"
                    []
                    [ darkMode' model
                    , if activeSetsType' == MM.MyShared then "active" else ""
                    ]
            , M.onClick $ MA.SwitchActiveSetsType MA.MyShared
            , M.type_ "button"
            , M.value_ "My Shared"
            ]
        , M.input_
            [ M.class_ . bemClass "Tab"
                $ BemClass
                    "Sets"
                    []
                    [ darkMode' model
                    , if activeSetsType' == MM.Shared then "active" else ""
                    ]
            , M.onClick $ MA.SwitchActiveSetsType MA.TheirShared
            , M.type_ "button"
            , M.value_ "Shared"
            ]
        ]
    ] ++ case model ^. activeSetsType of
        MM.Downloaded ->
            [ setUl
                (Right
                    $ model
                        ^.. sets.traversed._Right._Right._Right.to
                            (^. sharedSet.set).withIndex)
                MM.Downloaded
            , pageSwitcher
                (BemClass "Sets" [ darkMode' model ] [])
                (MA.Sets MA.Downloaded)
                (model ^. pagination.sets.downloaded.count)
                model
            ]
        MM.Local      ->
            [ setUl (Right $ model ^.. sets.traversed._Left.withIndex) MM.Local
            , pageSwitcher
                (BemClass "Sets" [ darkMode' model ] [])
                (MA.Sets MA.MyLocal)
                (model ^. pagination.sets.myLocal.count)
                model
            , M.form_
                [ M.class_ . bemClass "Form"
                    $ BemClass "Sets" [ "inline", darkMode' model ] [ "inline" ]
                , M.data_ "mark" "add-set-form"
                , M.onSubmit MA.AddSet
                ]
                [ M.input_
                    [ M.class_ . bemClass "FormField" $ BemClass "Sets" [] []
                    , M.name_ "_name"
                    , M.placeholder_ "Set name"
                    ]
                , M.input_
                    [ M.class_ . bemClass "Button"
                        $ BemClass "Sets" [ darkMode' model ] []
                    , M.type_ "submit"
                    , M.value_ "+"
                    ]
                ]
            ]
        MM.MyShared   ->
            [ setUl
                (Right $ model ^.. sets.traversed._Right._Left.to (^. set).withIndex)
                MM.MyShared
            , pageSwitcher
                (BemClass "Sets" [ darkMode' model ] [])
                (MA.Sets MA.MyShared)
                (model ^. pagination.sets.myShared.count)
                model
            ]
        MM.Shared     ->
            [ setUl (Left $ model ^. liteSharedSets) MM.Shared
            , pageSwitcher
                (BemClass "Sets" [ darkMode' model ] [])
                (MA.Sets MA.TheirShared)
                (model ^. pagination.sets.theirShared.count)
                model
            ])
  where
    setUl :: Either [LiteSharedSet] [(SetIx, Set)] -> MM.SetsType
        -> M.View MA.Action
    setUl (Left liteSharedSets') MM.Shared = M.ul_
        [ M.class_ $ bemClass "SetList" $ BemClass "Sets" [] []
        ]
        . map (\liteSharedSet -> M.li_
            [ M.class_ . bemClass "SetListItem" $ BemClass "Sets" [] []
            ]
            [ M.a_
                [ M.class_ . bemClass "SetListItemName" $ BemClass "Sets" [] []
                , M.onClick . MA.DownloadSharedSet $ liteSharedSet ^. sharedSetId
                ]
                [ M.text $ liteSharedSet ^. sharedSetName.to ms
                ]
            ])
        $ paginate
            (model ^. pagination.sets.theirShared.current)
            (model ^. settings.setsPageCount.to read)
            liteSharedSets'
    setUl (Right ixedSets)       setsType       = M.ul_
        [ M.class_ $ bemClass "SetList" $ BemClass "Sets" [] []
        ]
        . map (\(setIx, set') -> M.li_
            [ M.class_ . bemClass "SetListItem" $ BemClass "Sets" [] []
            ]
            ([ M.a_
                [ M.class_ . bemClass "SetListItemName" $ BemClass "Sets" [] []
                , M.onClick $ goSet setIx
                ]
                [ M.text $ set' ^. name.to ms
                ]
            , M.input_
                [ M.class_ . bemClass "Button" $ BemClass "Sets" [ darkMode' model ] []
                , M.onClick $ MA.DeleteSet setIx
                , M.type_ "button"
                , M.value_ "-"
                ]
            ] ++ case setsType of
                MM.Local -> [ M.input_
                        [ M.class_ . bemClass "Button"
                            $ BemClass "Sets" [ darkMode' model ] []
                        , M.onClick $ MA.ShareSet setIx
                        , M.type_ "button"
                        , M.value_ "Share"
                        ]
                    ]
                _        -> []
            ))
        $ paginate
            (model ^. pagination.sets.case setsType of
                    MM.Downloaded -> downloaded
                    MM.Local      -> myLocal
                    MM.MyShared   -> myShared
                    _             -> myLocal
                .current)
            (model ^. settings.setsPageCount.to read)
            ixedSets
    setUl _       _                             = M.ul_ [] []

    activeSetsType' = model ^. activeSetsType
