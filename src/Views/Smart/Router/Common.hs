{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Views.Smart.Router.Common
    ( router
    ) where

import Control.Lens ((^.), (^..))
import Control.Lens.Combinators (each, non, to)
import Control.Lens.TH (makeFieldsNoPrefix)
import Miso (View (), runRoute)
import Servant.API ((:<|>) ((:<|>)))

import Model.Action (Action ())
import Model.Model (LiteSet (), Memorizing (), Model (_uri), Settings ())
import Utils (BemClass (BemClass))
import Views.Dumb.Home.Common (home)
import Views.Smart.Memorizing.Common (memorizing')
import Views.Smart.Router.Utils (routes)
import Views.Smart.Set.Common (set)
import Views.Smart.Sets.Common (sets')
import Views.Smart.Settings.Common (settings')
import Views.Smart.Statistics.Common (statistics')


makeFieldsNoPrefix ''LiteSet
makeFieldsNoPrefix ''Memorizing
makeFieldsNoPrefix ''Model
makeFieldsNoPrefix ''Settings

router :: Model -> View Action
router model = either (const . home $ BemClass "ActivePage" [] []) id
    $ runRoute routes pages _uri model
  where
    memorizing'' = model ^. memorizing

    pages
       =    const (home $ BemClass "ActivePage" [] [])
       :<|> memorizing' (BemClass "ActivePage" [] [])
       :<|> (\setIx' -> set
        (BemClass
            "ActivePage"
            [
                if model ^. settings.activeSetIxs.non [].to (elem setIx' . map read)
                && length (memorizing'' ^.. liteSets.each.unitIxs)
                    < memorizing'' ^. initLiteSetsLen
                then "memorized"
                else ""
            ]
            [])
        setIx')
       :<|> sets' (BemClass "ActivePage" [] [])
       :<|> settings' (BemClass "ActivePage" [] [])
       :<|> statistics' (BemClass "ActivePage" [] [])
