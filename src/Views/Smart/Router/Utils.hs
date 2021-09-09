{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Views.Smart.Router.Utils
    ( goHome
    , goMemorizing
    , goSet
    , goSets
    , goSettings
    , routes
    ) where

import Data.Proxy (Proxy (Proxy))
import Miso (View ())
import Servant.API (Capture (), (:<|>) (), (:>))
import Servant.Links (linkURI, safeLink)

import Model.Action (Action (ChangeUri))
import Utils (SetIx ())


type Routes = Home :<|> Memorizing :<|> Set :<|> Sets :<|> Settings
type Home       = View Action
type Memorizing = "memorizing" :> View Action
type Set        = "set" :> Capture "id" Int :> View Action
type Sets       = "sets" :> View Action
type Settings   = "settings" :> View Action

routes :: Proxy Routes
routes = Proxy :: Proxy Routes

goHome, goMemorizing, goSets, goSettings :: Action
goHome       = ChangeUri . linkURI $ safeLink routes (Proxy :: Proxy Home)
goMemorizing = ChangeUri . linkURI $ safeLink routes (Proxy :: Proxy Memorizing)
goSets       = ChangeUri . linkURI $ safeLink routes (Proxy :: Proxy Sets)
goSettings   = ChangeUri . linkURI $ safeLink routes (Proxy :: Proxy Settings)

goSet :: SetIx -> Action
goSet setIx = ChangeUri . linkURI $ safeLink routes (Proxy :: Proxy Set) setIx
