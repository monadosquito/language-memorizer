{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.PageSwitcher.Common
    ( pageSwitcher
    ) where

import Miso (View (), aside_, class_, div_, input_, onClick, type_, value_)
import Miso.String (ms)

import Model.Action (Action (SwitchPage), Paginated (), PageSwitchWay (..))
import Utils (PagesCount (), BemClass (BemClass), bemClass)


pageSwitcher :: BemClass -> Paginated -> PagesCount -> View Action
pageSwitcher (BemClass parentName blockMods elemMods) paginated pagesCount = aside_
    [ class_ . bemClass "PageSwitcher"
        $ BemClass
            parentName
            (blockMods ++ [ if pagesCount < 2 then "hidden" else "" ])
            elemMods 
    ]
    [ div_
        [ class_ . bemClass "JumpPageButtonContainer" $ BemClass "PageSwitcher" [] []
        ]
        (map
            (\page -> input_
                [ class_ . bemClass "Button" $ BemClass "PageSwitcher" [] []
                , onClick $ SwitchPage (Jump page) paginated
                , type_ "button"
                , value_ . ms $ page + 1
                ])
            [ 0..pagesCount - 1 ])
    , div_
        [ class_ . bemClass "SwitchPageButtonContainer"
            $ BemClass "PageSwitcher" [] []
        ]
        [ input_
            [ class_ . bemClass "Button" $ BemClass "PageSwitcher" [] []
            , onClick $ SwitchPage First paginated
            , type_ "button"
            , value_ "|-"
            ]
        , input_
            [ class_ . bemClass "Button" $ BemClass "PageSwitcher" [] []
            , onClick $ SwitchPage Last paginated
            , type_ "button"
            , value_ "-|"
            ]
        , input_
            [ class_ . bemClass "Button" $ BemClass "PageSwitcher" [] []
            , onClick $ SwitchPage Previous paginated
            , type_ "button"
            , value_ "<"
            ]
        , input_
            [ class_ . bemClass "Button" $ BemClass "PageSwitcher" [] []
            , onClick $ SwitchPage Next paginated
            , type_ "button"
            , value_ ">"
            ]
        ]
    ]
