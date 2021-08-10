{-# LANGUAGE OverloadedStrings #-}

module Views.Smart.SignUp.Common
    ( signUp
    ) where

import Model.Action (Action (SignUp))
import Model.Model (Model ())
import Utils (BemClass (BemClass), bemClass, darkMode')

import qualified Miso as M


signUp :: BemClass -> Model -> M.View Action
signUp bemClass' model = M.main_
    [ M.class_ $ bemClass "SignUp" bemClass'
    ]
    [ M.form_
        [ M.class_ . bemClass "Form" $ BemClass "SignUp" [ darkMode' model ] []
        , M.data_ "mark" "sign-up-form"
        , M.onSubmit SignUp
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
        , M.label_ [] 
            [ M.input_
                [ M.class_ . bemClass "FormField" $ BemClass "Form" [] []
                , M.name_ "_name"
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
            [ M.class_ . bemClass "Button" $ BemClass "SignUp" [ darkMode' model ] []
            , M.type_ "submit"
            , M.value_ "Sign Up"
            ]
        ]
    ]
