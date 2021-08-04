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
import Utils (BemClass (BemClass), darkMode')
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
router model = either (const . home $ BemClass "ActivePage" [ darkMode' model ] []) id
    $ runRoute routes pages _uri model
  where
    memorizing'' = model ^. memorizing

    pages
       =    const (home $ BemClass "ActivePage" [ darkMode' model ] [])
       :<|> memorizing' (BemClass "ActivePage" [ darkMode' model ] [])
       :<|> (\setIx' -> set
        (BemClass
            "ActivePage"
            [ darkMode' model
            , if model ^. settings.activeSetIxs.non [].to (elem setIx' . map read)
                && length (memorizing'' ^.. liteSets.each.unitIxs)
                    < memorizing'' ^. initLiteSetsLen
                then "memorized"
                else ""
            ]
            [])
        setIx')
       :<|> sets' (BemClass "ActivePage" [ darkMode' model ] [])
       :<|> settings' (BemClass "ActivePage" [ darkMode' model ] [])
       :<|> statistics' (BemClass "ActivePage" [ darkMode' model ] [])
