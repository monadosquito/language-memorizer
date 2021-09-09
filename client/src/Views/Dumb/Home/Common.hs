{-# LANGUAGE OverloadedStrings #-}

module Views.Dumb.Home.Common
    ( home
    ) where

import Miso (View (), class_, details_, h1_, h2_, li_, main_, ol_, p_, summary_, text, ul_)

import Model.Action (Action ())
import Utils (BemClass (BemClass), bemClass)


home :: BemClass -> View Action
home bemClass' = main_
    [ class_ $ bemClass "Home" bemClass'
    ]
    [ p_ []
        [ h1_
            [ class_ . bemClass "Title" $ BemClass "Home" [] []
            ]
            [ text
                "This app is intended to help one memorize static language things (phrases, words, etc.) easier and faster."
            ]
        , h2_ 
            [ class_ . bemClass "Title" $ BemClass "Home" [] []
            ]
            [ text "Features:"
            ]
        , ul_ []
            [ li_
                [ class_ . bemClass "ListItem" $ BemClass "Home" [] []
                ]
                [ details_ []
                    [ ul_ []
                        [ li_
                            [ class_ . bemClass "ListItem" $ BemClass "Home" [] [ "nonbundled" ]
                            ]
                            [ text "A \"Set\" consists of its name and the Units one wants to memorize."
                            ]
                        , li_
                            [ class_ . bemClass "ListItem" $ BemClass "Home" [] [ "nonbundled" ]
                            ]
                            [ text
                                "A \"Unit\" consists of a text in the first language and a list of texts, which are translates of the text, in the second language."
                            ]
                        ]
                    , summary_
                        [ class_ . bemClass "BundleListItem" $ BemClass "Home" [] []
                        ]
                        [ text "Creating Sets and filling them with Units"
                        ]
                    ]
                ]
            , li_
                [ class_ . bemClass "ListItem" $ BemClass "Home" [] []
                ]
                [ details_ []
                    [ p_ []
                        [ text
                            "The \"Memorizing Mode\" page is something like interactive flashcards showing a randomly selected Unit text or one of Unit translates from a randomly selected Active Set while one is typing the answer."
                        ]
                    , summary_
                        [ class_ . bemClass "BundleListItem" $ BemClass "Home" [] []
                        ]
                        [ text "Memorizing Units via Memorizing Mode"
                        ]
                    ]
                ]
            , li_
                [ class_ . bemClass "ListItem" $ BemClass "Home" [] []
                ]
                [ details_ []
                    [ p_ []
                        [ text
                            "The \"Statistics\" page shows one's memorizing history (newest first) highlighting answered Units with green and shown Units or undone memorizing session with red."
                        ]
                    , summary_
                        [ class_ . bemClass "BundleListItem" $ BemClass "Home" [] []
                        ]
                        [ text "Viewing one's progress via Statistics"
                        ]
                    ]
                ]
            , li_
                [ class_ . bemClass "ListItem" $ BemClass "Home" [] [ "nonbundled" ]
                ]
                [ text "Sharing one's Sets"
                ]
            , li_
                [ class_ . bemClass "ListItem" $ BemClass "Home" [] [ "nonbundled" ]
                ]
                [ text "Downloading shared Sets rated with likes or dislikes"
                ]
            ]
        , details_ []
            [ ol_ []
                [ li_ []
                    [ text
                        "Create at least one Set by typing one's name in the input field and clicking on the \"+\" button on the \"Sets\" page."
                    ]
                , li_ []
                    [ text
                        "Create at least one Unit inside the Set on the \"Set\" page (click a Set's name on the \"Sets\" page to get there) by clicking the \"+\" button and filling in newly created Unit fields."
                    ]
                , li_ []
                    [ text
                        "Mark the Set as Active Set on the \"Settings\" page by clicking on its name and then the \"Save\" button."
                    ]
                ]
            , summary_
                [ class_ . bemClass "BundleListItem" $ BemClass "Home" [] []
                ]
                [ h2_
                    [ class_ . bemClass "Title" $ BemClass "Home" [] [ "inline" ]
                    ]
                    [ text "How to start:"
                    ]
                ]
            ]
        ] 
    ]
