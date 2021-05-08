module Model.UpdateModel
    ( updateModel
    ) where

import Miso (Effect (), noEff)

import Model.Action (Action (..))
import Model.Model (Model ())


updateModel :: Action -> Model -> Effect Action ()
updateModel DoNothing model = noEff model
