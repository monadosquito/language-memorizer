{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TypeOperators          #-}

module Views.Smart.Router.Common
    ( router
    ) where

import Miso (View (), runRoute)
import Servant.API ((:<|>) ((:<|>)))

import Model.Action (Action ())
import Model.Model (Model (_uri))
import Utils (BemClass (BemClass))
import Views.Dumb.Home.Common (home)
import Views.Smart.Router.Utils (routes)
import Views.Smart.Set.Common (set)
import Views.Smart.Sets.Common (sets')
import Views.Smart.Settings.Common (settings')


router :: Model -> View Action
router model = either (const . home $ BemClass "ActivePage" [] []) id
    $ runRoute routes pages _uri model
  where
    pages
       =    const (home $ BemClass "ActivePage" [] [])
       :<|> set (BemClass "ActivePage" [] [])
       :<|> sets' (BemClass "ActivePage" [] [])
       :<|> settings' (BemClass "ActivePage" [] [])
