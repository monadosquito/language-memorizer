{-# LANGUAGE OverloadedStrings #-}

module Views.Smart.SignIn.Common
    ( signIn
    ) where

import Model.Action (Action (SignIn))
import Model.Model (Model ())
import Utils (BemClass (BemClass), bemClass, darkMode')

import qualified Miso as M


signIn :: BemClass -> Model -> M.View Action
signIn bemClass' model = M.main_
    [ M.class_ $ bemClass "SignIn" bemClass'
    ]
    [ M.form_
        [ M.class_ . bemClass "Form" $ BemClass "SignIn" [ darkMode' model ] []
        , M.data_ "mark" "sign-in-form"
        , M.onSubmit SignIn
        ]
        [ M.label_ [] 
            [ M.input_
                [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                , M.name_ "_email"
                ]
            , M.span_ []
                [ M.text "Email"
                ]
            ]
        , M.label_
            [ M.hidden_ True
            ]
            [ M.input_
                [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                , M.name_ "_name"
                , M.value_ ""
                ]
            , M.span_ []
                [ M.text "Name"
                ]
            ]
        , M.label_ [] 
            [ M.input_
                [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                , M.name_ "_password"
                ]
            , M.span_ []
                [ M.text "Password"
                ]
            ]
        , M.input_
            [ M.class_ . bemClass "Button" $ BemClass "SignIn" [ darkMode' model ] []
            , M.type_ "submit"
            , M.value_ "Sign In"
            ]
        ]
    ]
