{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Model.UpdateModel
    ( updateModel
    ) where

import Control.Lens ((%~), (&), (+~), (-~), (.~), (^.), (^..), (^?), _1, _2)
import Control.Lens.Combinators (_head, ifiltered, ix, non, each, filtered, to, withIndex)
import Control.Lens.Extras (is)
import Control.Lens.Prism (_Just)
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Traversal (traversed)
import Control.Monad (replicateM)
import Data.Aeson (encode)
import Data.Maybe (listToMaybe)
import Language.Javascript.JSaddle ((!), (#), (<#), create, fun, jsg, jsg2, valToNumber, valToStr)
import Miso.String (fromMisoString, ms)

import Utils (formData, listedStep, pagesCount, setResultsIsDone)

import qualified Miso as M

import qualified Model.Model as MM
import qualified Model.Action as MA


makeFieldsNoPrefix ''MM.EditedSet
makeFieldsNoPrefix ''MM.LiteSet
makeFieldsNoPrefix ''MM.Memorizing
makeFieldsNoPrefix ''MM.Model
makeFieldsNoPrefix ''MM.Pages
makeFieldsNoPrefix ''MM.Pagination
makeFieldsNoPrefix ''MM.Set
makeFieldsNoPrefix ''MM.SetResult
makeFieldsNoPrefix ''MM.SetResultStep
makeFieldsNoPrefix ''MM.Settings
makeFieldsNoPrefix ''MM.Unit
makeFieldsNoPrefix ''MM.LanguageMemorizer

updateModel :: MA.Action -> MM.Model -> M.Effect MA.Action MM.Model
updateModel MA.AddSet                                                model = model M.<# do
    (set:_) <- formData (ms "add-set-form") True :: M.JSM [MM.Set]
    pure $ MA.UpdateSets Nothing set
updateModel (MA.AddTranslate unitIx')                                model = M.noEff
    $ model&sets.ix (model ^. activeSetIx).units._Just.ix unitIx'.translates
        %~ (++ [ ms "" ])
updateModel MA.AddUnit                                               model = model M.<# do
    pure . MA.UpdateUnits (model ^. activeSetIx) $ MM.Unit (ms "") [ ms "" ]
updateModel (MA.ChangeUri uri')                                      model =
    (model&menuIsVisible .~ False) M.<# (M.pushURI uri' >> pure MA.DoNothing)
updateModel (MA.CheckAnswer MA.Text' text')                          model =
    (model&memorizing.answer .~ text')
        M.<# (pure
            $ if model
                ^? sets.ix (memorizing' ^. setIx)
                    .units._Just.ix (memorizing' ^. unitIx).text
                ^. non (ms "").to (== text')
            then MA.SelectRandomMemorizingUnit
            else MA.DoNothing)
  where
    memorizing' = model ^. memorizing
updateModel (MA.CheckAnswer MA.Translates translate)                 model =
    (model&memorizing.answer .~ translate)
        M.<# (pure . maybe MA.DoNothing (const MA.SelectRandomMemorizingUnit)
            $ model
                ^? sets.ix (model ^. memorizing.setIx)
                    .units._Just.ix (model ^. memorizing.unitIx)
                    .translates.each.filtered (== translate))
updateModel (MA.DeleteSet setIx')                                    model = (model
    &sets %~ (^.. traversed.ifiltered (\setIx'' _ -> setIx'' /= setIx'))
    &pagination.sets.count -~  isLastPageElem
    &pagination.sets.current -~ isLastPageElem
    &sets %~ (^.. traversed.ifiltered (\setIx'' _ -> setIx'' /= setIx'))
    ) M.<# pure MA.SaveSets
  where
    isLastPageElem =
        (if model ^. sets.to length
            == (model ^. pagination.sets.count - 1)
                * model ^. settings.setsPageCount.to read + 1
        then 1
        else 0)
updateModel (MA.DeleteTranslate unitIx' transIx)                     model = M.noEff
    $ model&sets.ix (model ^. activeSetIx).units._Just.ix unitIx'.translates
        %~ (^.. traversed.ifiltered (\transIx' _ -> transIx' /= transIx))
updateModel (MA.DeleteUnit unitIx')                                  model = M.batchEff
    (model
        &pagination.units.ix activeSetIx'.count -~ isLastPageElem
        &pagination.units.ix activeSetIx'.current -~ isLastPageElem
        &sets.ix activeSetIx'.units._Just %~ (^.. traversed.ifiltered
            (\unitIx'' _ -> unitIx'' /= unitIx'))
    )
    [ pure MA.SaveSets
    , pure . MA.UpdatePagination . MA.Part . MA.Units $ model ^. activeSetIx
    ]
  where
    isLastPageElem =
        (if model ^? sets.ix activeSetIx'.units._Just.to length ^. non (-1)
            == (model ^? pagination.units.ix activeSetIx'.count ^. non 0.to (subtract 1))
                * model ^. settings.unitsPageCount.to read + 1
        then 1
        else 0)
    activeSetIx'   = model ^. activeSetIx
updateModel MA.DoNothing                                             model = M.noEff model
updateModel (MA.EditSet MA.Name _ name')                             model =
    M.noEff $ model&editedSet.name .~ name'
updateModel (MA.EditSet editedUnitPart unitIx' editedUnitPartVal)    model = M.noEff
    $ newModel
        &(if newEditedUnit
            == model
                ^? sets.ix (model ^. activeSetIx).units._Just.ix unitIx' ^. non zeroUnit
            then editedSet.ixedUnits %~ (^.. traversed.filtered
                (\(unitIx'', _) -> unitIx'' /= unitIx'))
            else editedSet.ixedUnits %~ (^.. traversed.to (\ixedEditedUnit ->
                if ixedEditedUnit ^. _1 == unitIx'
                then ixedEditedUnit&_2 .~ newEditedUnit
                else ixedEditedUnit)))
  where
    editedUnit    = newModel ^.. editedSet.ixedUnits.traversed.filtered
        (\(unitIx'', _) -> unitIx'' == unitIx') ^? to listToMaybe._Just._2 ^. non zeroUnit
    newEditedUnit = case editedUnitPart of
        MA.UnitText              -> editedUnit&text .~ editedUnitPartVal
        MA.UnitTranslate transIx -> editedUnit&translates.ix transIx .~ editedUnitPartVal
        _                        -> editedUnit
    newModel      =
        (if null $ model ^.. editedSet.ixedUnits.traversed.filtered
            (\(unitIx'', _) -> unitIx'' == unitIx')
        then model&editedSet.ixedUnits %~ (++
            [
                ( unitIx'
                , model
                    ^? sets.ix (model ^. activeSetIx).units._Just.ix unitIx'
                    ^. non zeroUnit
                )
            ])
        else model)
    zeroUnit      = MM.Unit (ms "") []
updateModel MA.FailMemorizingStep                                    model = (model
    &memorizing.pause .~ True
    &memorizing.progress %~ (++ [ False ])
    &statistics .~ newStatistics
    ) M.<# (M.setLocalStorage (ms "statistics") newStatistics >> pure MA.DoNothing)
  where
    memorizing'   = model ^. memorizing
    newStatistics = model
        ^. statistics&_head.traversed
            .filtered ((== memorizing' ^. setIx) . (^. setIx)).steps
        %~ (++ [ MM.SetResultStep False $ memorizing' ^. unitIx ])
updateModel (MA.HandleUri uri')                                      model =
    M.noEff $ model&uri .~ uri'
updateModel (MA.RefreshSet setIx')                                   model = (model
    &editedSet.ixedUnits .~ []
    &editedSet.name .~ model ^? sets.ix setIx'.name ^. non (ms "")
    &activeSetIx .~ setIx'
    ) M.<# pure MA.DoNothing
updateModel MA.RepeatMemorizing                                      model = (model
    &memorizing.answer .~ ms ""
    &memorizing.initLiteSetsLen .~ liteSets' ^.. each.unitIxs.each ^. to length
    &memorizing.liteSets .~ liteSets'
    &memorizing.progress .~ []
    &statistics %~ (\statistics' ->
        if is _Just activeSetIxs'
        then
            if setResultsIsDone model
            then setResults : statistics'
            else setResults : tail statistics'
        else statistics'))
    M.<# pure MA.SelectRandomMemorizingUnit
  where
    activeSetIxs' = model ^. settings.activeSetIxs
    liteSets'     = (model
        ^.. sets.traversed.ifiltered
            (\setIx' set' ->
                elem (show setIx') (model ^? settings.activeSetIxs._Just ^. non [])
                    && (not . null $ set' ^? units._Just ^. non []))
            .withIndex)
        ^.. traversed.to (\(setIx', set') ->
            MM.LiteSet setIx' [ 0..set' ^. units.non [].to ((subtract 1) . length) ])
    setResults    = activeSetIxs' ^.. _Just.each.to ((`MM.SetResult` []) . read)
updateModel MA.SaveSet                                               model = (model
    &editedSet.ixedUnits .~ []
    &sets.ix activeSetIx'.units._Just .~ model
        ^.. sets.ix activeSetIx'.units._Just.traversed.withIndex.to
            (\(editedUnitIx, editedUnit) -> (model ^.. editedSet.ixedUnits.traversed.filtered
                (\(editedUnitIx', _) -> editedUnitIx' == editedUnitIx))
        ^? _head . _2
        ^. non editedUnit)
    &sets.ix activeSetIx'.name .~ model ^. editedSet.name
    ) M.<# pure MA.SaveSets
  where
    activeSetIx' = model ^. activeSetIx
updateModel MA.SaveSets                                              model = model M.<#
    (M.setLocalStorage (ms "sets") (model ^. sets) >> pure MA.DoNothing)
updateModel MA.SaveSettings                                          model = model M.<# do
    (settings':_) <- formData (ms "edit-settings-form") False :: M.JSM [MM.Settings]
    M.setLocalStorage (ms "settings") settings'
    pure $ MA.UpdateSettings settings'
updateModel MA.SelectRandomMemorizingUnit                            model = model M.<# do
    [ randomNum, randomNum1, randomNum2 ] <- replicateM 3
        $ valToNumber =<< (jsg "Math" # "random" $ ())
    let
        liteSet      = model
            ^? memorizing.liteSets.ix liteSetIx' ^. non (MM.LiteSet (-1) [])
        liteSetIx'   = randomIx randomNum $ model ^. memorizing.liteSets.to length
        liteUnitIx   = randomIx randomNum1
            $ model ^? memorizing.liteSets.ix liteSetIx'.unitIxs ^. non [].to length
        translateIx' = randomIx randomNum2
            $ model ^? sets.ix (liteSet ^. setIx).units._Just.ix unitIx'.translates
                ^. non [].to length
        unitIx'      = liteSet ^? unitIxs.ix liteUnitIx ^. non (-1)
    pure $ MA.UpdateMemorizing liteSetIx' liteUnitIx translateIx'
  where
    randomIx :: Double -> Int -> Int
    randomIx randomNum' length' = floor $ randomNum' * fromIntegral length'
updateModel (MA.ShowAnswer MA.Text')                                 model = (model
    &memorizing.answer .~ model
            ^? sets.ix (memorizing' ^. setIx).units._Just.ix (memorizing' ^. unitIx).text
            ^. non (ms ""))
        M.<# pure MA.FailMemorizingStep
  where
    memorizing' = model ^. memorizing
updateModel (MA.ShowAnswer MA.Translates)                            model = (model
    &memorizing.answer .~ model
            ^? sets.ix (memorizing' ^. setIx).units._Just.ix (memorizing' ^. unitIx)
                .translates.ix (memorizing' ^. translateIx)
            ^. non (ms ""))
        M.<# pure MA.FailMemorizingStep
  where
    memorizing' = model ^. memorizing
updateModel MA.SignIn                                                model = model M.<# do
    (langMemorizer':_) <- formData (ms "sign-in-form") True
        :: M.JSM [MM.LanguageMemorizer]
    fetchOptions <- create
    fetchOptions <# "body" $ ms $ encode langMemorizer'
    fetchHeaders <- create
    fetchHeaders <# "Content-Type" $ "application/json"
    fetchOptions <# "headers" $ fetchHeaders
    fetchOptions <# "method" $ "POST"
    resp <- jsg2 "fetch" "http://localhost:8080/sign-in" fetchOptions
    _ <- resp # "then"
        $ fun $ \_ _ args -> do
            case args of
                [ resp1 ] -> do
                    _ <- (resp1 # "text" $ ()) # "then" $ fun $ \_ _ args1 -> do
                        case args1 of
                            [ jsValAuthToken ] -> do
                                authToken <- fromMisoString <$> valToStr jsValAuthToken
                                _ <- jsg "localStorage" # "setItem"
                                    $ [ "authToken", authToken ]
                                pure ()
                            _                  -> pure ()

                    pure ()
                _         -> pure ()
            pure ()
    pure MA.DoNothing
updateModel MA.SignOut                                               model = model M.<#
    (M.removeLocalStorage (ms "authToken") >> pure MA.DoNothing)
updateModel MA.SignUp                                                model = model M.<# do
    (langMemorizer':_) <- formData (ms "sign-up-form") True
        :: M.JSM [MM.LanguageMemorizer]
    fetchOptions <- create
    fetchOptions <# "body" $ ms $ encode langMemorizer'
    fetchHeaders <- create
    fetchHeaders <# "Content-Type" $ "application/json"
    fetchOptions <# "headers" $ fetchHeaders
    fetchOptions <# "method" $ "POST"
    _ <- jsg2 "fetch" "http://localhost:8080/sign-up" fetchOptions
    pure MA.DoNothing
updateModel (MA.SwitchPage MA.First MA.Sets)                         model = M.noEff
    $ model&pagination.sets.current .~ 0
updateModel (MA.SwitchPage (MA.Jump page) MA.Sets)                   model = M.noEff
    $ model&pagination.sets.current .~ page
updateModel (MA.SwitchPage MA.Last MA.Sets)                          model = M.noEff
    $ model&pagination.sets.current .~ model ^. pagination.sets.count.to (subtract 1)
updateModel (MA.SwitchPage MA.Next MA.Sets)                          model = M.noEff $
    if model ^. pagination.sets.current < model ^. pagination.sets.count.to (subtract 1)
    then model&pagination.sets.current +~ 1
    else model
updateModel (MA.SwitchPage MA.Previous MA.Sets)                      model = M.noEff $
    if model ^. pagination.sets.current == 0
    then model
    else model&pagination.sets.current -~ 1
updateModel (MA.SwitchPage MA.First MA.Statistics)                   model =
    M.noEff $ model&pagination.statistics.current .~ 0
updateModel (MA.SwitchPage (MA.Jump page) MA.Statistics)             model =
    M.noEff $ model&pagination.statistics.current .~ page
updateModel (MA.SwitchPage MA.Last MA.Statistics)                    model =
    M.noEff
        $ model
            &pagination.statistics.current .~ model ^. pagination.sets.count.to
                (subtract 1)
updateModel (MA.SwitchPage MA.Next MA.Statistics)                    model =
    M.noEff $ if model ^. pagination.statistics.current
        < model ^. pagination.statistics.count.to (subtract 1)
    then model&pagination.statistics.current +~ 1
    else model
updateModel (MA.SwitchPage MA.Previous MA.Statistics)                model =
    M.noEff $ if model ^. pagination.statistics.current == 0
    then model
    else model&pagination.statistics.current -~ 1
updateModel (MA.SwitchPage MA.First (MA.Units setIx'))                     model =
    (model&pagination.units.ix setIx'.current .~ 0) M.<# pure (MA.RefreshSet setIx')
updateModel (MA.SwitchPage (MA.Jump page) (MA.Units setIx'))               model =
    (model&pagination.units.ix setIx'.current .~ page) M.<# pure (MA.RefreshSet setIx')
updateModel (MA.SwitchPage MA.Last (MA.Units setIx'))                      model =
    (model&pagination.units.ix setIx'.current .~ (model
        ^? pagination.units.ix setIx'.count
        ^. non 1.to (subtract 1))) M.<# pure (MA.RefreshSet setIx')
updateModel (MA.SwitchPage MA.Next (MA.Units setIx'))                model =
    (if model ^? pagination.units.ix setIx'.current ^. non (-1)
        <
        (model ^? pagination.units.ix setIx'.count ^. non 1.to (subtract 1))
    then model&pagination.units.ix setIx'.current +~ 1
    else model) M.<# pure (MA.RefreshSet setIx')
updateModel (MA.SwitchPage MA.Previous (MA.Units setIx'))            model =
    (if model ^? pagination.units.ix setIx'.current ^. non (-1) == 0
    then model
    else model&pagination.units.ix setIx'.current -~ 1) M.<# pure (MA.RefreshSet setIx')
updateModel MA.ToggleMenuVisibility                                  model =
    (model&menuIsVisible %~ not)
        M.<# ((M.getBody ! "classList" # "toggle" $ [ "covered" ]) >> pure MA.DoNothing)
updateModel (MA.UpdateMemorizing liteSetIx' tmpUnitIx' translateIx') model = (model
    &memorizing .~ newMemorizing
    &statistics .~ newStatistics
    ) M.<# do
        M.setLocalStorage (ms "memorizing") newMemorizing
        M.setLocalStorage (ms "statistics") newStatistics
        pure . MA.UpdatePagination $ MA.Part MA.Statistics
  where
    memorizing'   = model ^. memorizing
    newMemorizing = (memorizing'
        &answer .~ ms ""
        &liteSets.ix liteSetIx'.unitIxs %~ (^.. traversed.ifiltered
            (\tmpUnitIx'' _ -> tmpUnitIx'' /= tmpUnitIx'))
        &pause .~ False
        &progress %~ (++ listedStep memorizing' True)
        &setIx .~ memorizing'
            ^? liteSets.ix liteSetIx' ^. non (MM.LiteSet (-1) []).setIx
        &translateIx .~ translateIx'
        &unitIx .~ memorizing'
            ^? liteSets.ix liteSetIx'.unitIxs.ix tmpUnitIx' ^. non (-1))
        &liteSets %~ (^.. traversed.filtered ((not . null) . (^. unitIxs)))
    newStatistics = model ^. statistics&_head.traversed
            .filtered ((== memorizing' ^. setIx) . (^. setIx)).steps
        %~ (++ (listedStep memorizing' . MM.SetResultStep True $ memorizing' ^. unitIx))
updateModel (MA.UpdatePagination (MA.Part MA.Sets))                  model = M.noEff
    $ model
        &pagination.sets.count .~ (pagesCount (model ^. settings.setsPageCount.to read)
            $ (model ^. sets.to length))
        &pagination.units %~ (++ [ MM.Pages 0 0 ])
updateModel (MA.UpdatePagination (MA.Part MA.Statistics))            model = M.noEff
    $ model
        &pagination.statistics.count .~ (pagesCount
            (model ^. settings.statisticsPageCount.to read)
            $ (model ^. statistics.to length))
updateModel (MA.UpdatePagination (MA.Part (MA.Units setIx')))        model = M.noEff
    $ model
        &pagination.units.ix setIx'.count .~ (pagesCount
            (model ^. settings.unitsPageCount.to read)
            $ (model ^? sets.ix setIx'.units._Just.to length) ^. non (-1))
updateModel (MA.UpdatePagination MA.Whole)                           model =
    (model&pagination.units.traversed.withIndex %~ (\setPages@(setIx', _) ->
            setPages&_2.count .~ (pagesCount (model ^. settings.unitsPageCount.to read)
                $ (model ^? sets.ix setIx'.units._Just.to length) ^. non (-1))))
        M.<# pure (MA.UpdatePagination $ MA.Part MA.Sets)
updateModel (MA.UpdateSets (Just setIx') set)                        model = M.batchEff
    (model&sets.ix setIx' .~ set)
    [ pure MA.SaveSets, pure . MA.UpdatePagination $ MA.Part MA.Sets ]
updateModel (MA.UpdateSets Nothing      set)                         model = M.batchEff
    (model&sets %~ (++ [ set ]))
    [ pure MA.SaveSets, pure . MA.UpdatePagination $ MA.Part MA.Sets ]
updateModel (MA.UpdateSettings settings')                            model = M.batchEff
    (model&settings .~ settings')
    [ pure MA.RepeatMemorizing, pure $ MA.UpdatePagination MA.Whole ]
updateModel (MA.UpdateUnits setIx' unit)                             model = M.batchEff
    (model&sets.ix setIx'.units.non [] %~ (++ [ unit ]))
    [ pure MA.SaveSets
    , pure . MA.UpdatePagination . MA.Part $ MA.Units setIx'
    , if model ^. settings.activeSetIxs.non [].to ((elem setIx') . map read)
        then pure MA.RepeatMemorizing
        else pure MA.DoNothing
    ]
