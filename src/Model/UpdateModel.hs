{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Model.UpdateModel
    ( updateModel
    ) where

import Control.Lens ((%~), (&), (+~), (-~), (.~), (^.), (^..), (^?), _1, _2)
import Control.Lens.Combinators (_head, filtered, ifiltered, ix, non, to, withIndex)
import Control.Lens.Prism (_Just)
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Traversal (traversed)
import Data.Maybe (listToMaybe)
import Miso.String (ms)

import Utils (formData, pagesCount)

import qualified Miso as M

import qualified Model.Model as MM
import qualified Model.Action as MA


makeFieldsNoPrefix ''MM.EditedSet
makeFieldsNoPrefix ''MM.Model
makeFieldsNoPrefix ''MM.Pagination
makeFieldsNoPrefix ''MM.Set
makeFieldsNoPrefix ''MM.Settings
makeFieldsNoPrefix ''MM.Pages
makeFieldsNoPrefix ''MM.Unit

updateModel :: MA.Action -> MM.Model -> M.Effect MA.Action MM.Model
updateModel MA.AddSet                                             model = model M.<# do
    (set':_) <- formData (ms "add-set-form") True :: M.JSM [MM.Set]
    pure $ MA.UpdateSets Nothing set'
updateModel MA.AddUnit                                            model = model M.<# do
    pure . MA.UpdateUnits (model ^. activeSetIx) $ MM.Unit (ms "") [ ms "" ]
updateModel (MA.AddTranslate unitIx)                              model = M.noEff $ model
    &sets.ix (model ^. activeSetIx).units._Just.ix unitIx.translates %~ (++ [ ms "" ])
updateModel (MA.ChangeUri uri')                                   model =
    model M.<# (M.pushURI uri' >> pure MA.DoNothing)
updateModel (MA.DeleteSet setIx)                                  model = (model
    &pagination.sets.count -~  isLastPageElem
    &pagination.sets.current -~ isLastPageElem
    &sets %~ (^.. traversed.ifiltered (\setIx' _ -> setIx' /= setIx))
    ) M.<# pure MA.SaveSets
  where
    isLastPageElem =
        (if model ^. sets.to length
            == (model ^. pagination.sets.count - 1)
                * model ^. settings.setsPageCount.to read + 1
        then 1
        else 0)
updateModel (MA.DeleteTranslate unitIx transIx)                   model = M.noEff $ model
    &sets.ix (model ^. activeSetIx).units._Just.ix unitIx.translates
        %~ (^.. traversed.ifiltered (\transIx' _ -> transIx' /= transIx))
updateModel (MA.DeleteUnit unitIx)                                model = M.batchEff
    (model
        &pagination.units.ix activeSetIx'.count -~ isLastPageElem
        &pagination.units.ix activeSetIx'.current -~ isLastPageElem
        &sets.ix activeSetIx'.units._Just %~ (^.. traversed.ifiltered
            (\unitIx' _ -> unitIx' /= unitIx))
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
updateModel MA.DoNothing                                          model = M.noEff model
updateModel (MA.EditSet MA.Name _ name')                          model =
    M.noEff $ model&editedSet.name .~ name'
updateModel (MA.EditSet editedUnitPart unitIx editedUnitPartVal)  model = M.noEff
    $ newModel
        &(if newEditedUnit
            == model
                ^? sets.ix (model ^. activeSetIx).units._Just.ix unitIx ^. non zeroUnit
            then editedSet.ixedUnits %~ (^.. traversed.filtered
                (\(unitIx', _) -> unitIx' /= unitIx))
            else editedSet.ixedUnits %~ (^.. traversed.to (\ixedEditedUnit ->
                if ixedEditedUnit ^. _1 == unitIx
                then ixedEditedUnit&_2 .~ newEditedUnit
                else ixedEditedUnit)))
  where
    editedUnit    = newModel ^.. editedSet.ixedUnits.traversed.filtered
        (\(unitIx', _) -> unitIx' == unitIx) ^? to listToMaybe._Just._2 ^. non zeroUnit
    newEditedUnit = case editedUnitPart of
        MA.UnitText              -> editedUnit&text .~ editedUnitPartVal
        MA.UnitTranslate transIx -> editedUnit&translates.ix transIx .~ editedUnitPartVal
        _                        -> editedUnit
    newModel      =
        (if null $ model ^.. editedSet.ixedUnits.traversed.filtered
            (\(unitIx', _) -> unitIx' == unitIx)
        then model&editedSet.ixedUnits %~ (++
            [
                ( unitIx
                , model
                    ^? sets.ix (model ^. activeSetIx).units._Just.ix unitIx
                    ^. non zeroUnit
                )
            ])
        else model)
    zeroUnit      = MM.Unit (ms "") []
updateModel (MA.HandleUri uri')                                   model = M.noEff $ model
    &uri .~ uri'
updateModel (MA.RefreshSet setIx')                                model = (model
    &editedSet.ixedUnits .~ []
    &editedSet.name .~ model ^? sets.ix setIx'.name ^. non (ms "")
    &activeSetIx .~ setIx'
    ) M.<# pure MA.DoNothing
updateModel MA.SaveSet                                            model = (model
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
updateModel MA.SaveSets                                           model = model M.<#
    (M.setLocalStorage (ms "sets") (model ^. sets) >> pure MA.DoNothing)
updateModel MA.SaveSettings                                       model = model M.<# do
    (settings':_) <- formData (ms "edit-settings-form") False :: M.JSM [MM.Settings]
    M.setLocalStorage (ms "settings") settings'
    pure $ MA.UpdateSettings settings'
updateModel (MA.SwitchPage MA.First MA.Sets)                      model = M.noEff $ model
    &pagination.sets.current .~ 0
updateModel (MA.SwitchPage (MA.Jump page) MA.Sets)                model = M.noEff $ model
    &pagination.sets.current .~ page
updateModel (MA.SwitchPage MA.Last MA.Sets)                       model = M.noEff $ model
    &pagination.sets.current .~ model ^. pagination.sets.count.to (subtract 1)
updateModel (MA.SwitchPage MA.Next MA.Sets)                       model = M.noEff $
    if model ^. pagination.sets.current < model ^. pagination.sets.count.to (subtract 1)
    then model&pagination.sets.current +~ 1
    else model
updateModel (MA.SwitchPage MA.Previous MA.Sets)                   model = M.noEff $
    if model ^. pagination.sets.current == 0
    then model
    else model&pagination.sets.current -~ 1
updateModel (MA.SwitchPage MA.First (MA.Units setIx))             model =
    (model&pagination.units.ix setIx.current .~ 0) M.<# pure (MA.RefreshSet setIx)
updateModel (MA.SwitchPage (MA.Jump page) (MA.Units setIx))       model =
    (model&pagination.units.ix setIx.current .~ page) M.<# pure (MA.RefreshSet setIx)
updateModel (MA.SwitchPage MA.Last (MA.Units setIx))              model =
    (model&pagination.units.ix setIx.current .~ (model
        ^? pagination.units.ix setIx.count
        ^. non 1.to (subtract 1))) M.<# pure (MA.RefreshSet setIx)
updateModel (MA.SwitchPage MA.Next (MA.Units setIx))              model =
    (if model ^? pagination.units.ix setIx.current ^. non (-1)
        <
        (model ^? pagination.units.ix setIx.count ^. non 1.to (subtract 1))
    then model&pagination.units.ix setIx.current +~ 1
    else model) M.<# pure (MA.RefreshSet setIx)
updateModel (MA.SwitchPage MA.Previous (MA.Units setIx))          model =
    (if model ^? pagination.units.ix setIx.current ^. non (-1) == 0
    then model
    else model&pagination.units.ix setIx.current -~ 1) M.<# pure (MA.RefreshSet setIx)
updateModel (MA.UpdatePagination (MA.Part MA.Sets))               model = M.noEff $ model
    &pagination.sets.count .~ (pagesCount (model ^. settings.setsPageCount.to read)
        $ (model ^. sets.to length))
    &pagination.units %~ (++ [ MM.Pages 0 0 ])
updateModel (MA.UpdatePagination (MA.Part (MA.Units setIx)))      model = M.noEff $ model
    &pagination.units.ix setIx.count .~ (pagesCount
        (model ^. settings.unitsPageCount.to read)
        $ (model ^? sets.ix setIx.units._Just.to length) ^. non (-1))
updateModel (MA.UpdatePagination MA.Whole)                        model =
    (model&pagination.units.traversed.withIndex %~ (\setPages@(setIx, _) ->
            setPages&_2.count .~ (pagesCount (model ^. settings.unitsPageCount.to read)
                $ (model ^? sets.ix setIx.units._Just.to length) ^. non (-1))))
        M.<# pure (MA.UpdatePagination $ MA.Part MA.Sets)
updateModel (MA.UpdateSets (Just setIx) set)                      model = M.batchEff
    (model&sets.ix setIx .~ set)
    [ pure MA.SaveSets, pure . MA.UpdatePagination $ MA.Part MA.Sets ]
updateModel (MA.UpdateSets Nothing      set)                      model = M.batchEff
    (model&sets %~ (++ [ set ]))
    [ pure MA.SaveSets, pure . MA.UpdatePagination $ MA.Part MA.Sets ]
updateModel (MA.UpdateSettings settings')                         model =
    (model&settings .~ settings') M.<# pure (MA.UpdatePagination MA.Whole)
updateModel (MA.UpdateUnits setIx unit)                           model = M.batchEff
    (model&sets.ix setIx.units.non [] %~ (++ [ unit ]))
    [ pure MA.SaveSets, pure . MA.UpdatePagination . MA.Part $ MA.Units setIx ]
