{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Avatar.Common
    ( avatar
    ) where

import Control.Lens ((^.), makeFieldsNoPrefix, to)
import Miso (View (), button_, class_, div_, onClick, text)

import Common (LanguageMemorizer ())
import Model.Action (Action (SignOut))
import Model.Model (Model ())
import Utils (BemClass (BemClass), bemClass, darkMode')


makeFieldsNoPrefix ''LanguageMemorizer
makeFieldsNoPrefix ''Model

avatar :: BemClass -> Model -> View Action
avatar bemClass' model = div_
    [ class_ $ bemClass "Avatar" bemClass'
    ]
    [ (model ^. langMemorizerName.to (maybe
        (div_ []
            [ text "Stranger"
            ])
        (\langMemorizerName' -> div_ []
            [ text langMemorizerName'
            , button_
                [ class_ . bemClass "Button" $ BemClass "Avatar" [ darkMode' model ] []
                , onClick SignOut
                ]
                [ text "Sign out"
                ]
            ])))
    ]
