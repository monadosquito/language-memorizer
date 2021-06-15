{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Model.UpdateModel
    ( updateModel
    ) where

import Control.Lens ((%~), (&), (.~), (^.), (^..), (^?))
import Control.Lens.Combinators (ix, non, ifiltered)
import Control.Lens.Traversal (traversed)
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Monad (void)
import Language.Javascript.JSaddle (JSVal (), (!), (#), (<#), valToNumber, valToStr)
import Miso.String (fromMisoString, ms)

import Model.Action (Action (..))
import Model.Model (Model (), Set (Set), Unit ())
import Utils (formData)

import qualified Miso as M


makeFieldsNoPrefix ''Model
makeFieldsNoPrefix ''Set
makeFieldsNoPrefix ''Unit

updateModel :: Action -> Model -> M.Effect Action Model
updateModel AddSet                        model = model M.<# do
    (set:_) <- formData (ms "add-set-form") True :: M.JSM [Set]
    pure $ UpdateSets Nothing set
updateModel AddUnit                       model = model M.<# do
    editUnitFormsContainer <- M.getElementById $ ms "editUnitFormsContainer"
    unitElem <- editUnitFormsContainer ! "children" ! "0" # "cloneNode" $ True
    unitElem ! "children" ! "0" ! "dataset" <# "mark" $ [ "edit-unit-form" ]
    unitElem ! "style" <# "display" $ [ "list-item" ]
    _ <- editUnitFormsContainer # "appendChild" $ [ unitElem ]
    pure DoNothing
updateModel (ChangeUri uri')              model =
    model M.<# (M.pushURI uri' >> pure DoNothing)
updateModel (DeleteSet setIx)             model = (model
    &sets %~ (^.. traversed.ifiltered (\setIx' _ -> setIx' /= setIx))) M.<# pure SaveSets
updateModel DoNothing                     model = M.noEff model
updateModel (HandleUri uri')              model = M.noEff $ model&uri .~ uri'
updateModel (RefreshSet setIx)            model = model M.<# do
    editSetPseudoForm <- M.getElementById $ ms "editSetPseudoForm"
    M.addEventListener editSetPseudoForm (ms "input") $ const toggleSaveSetButton
    editUnitFormsConstainer <- M.getElementById $ ms "editUnitFormsContainer"
    M.addEventListener editUnitFormsConstainer (ms "click")
        $ \event -> do
            mark <- valToStr $ event ! "target" ! "dataset" ! "mark"
            case fromMisoString mark of
                "add-translate"    -> addTranslate event >> toggleSaveSetButton
                "delete-translate" -> deleteTranslate event >> toggleSaveSetButton
                "delete-unit"      -> do
                    _ <- event ! "target" ! "parentNode" ! "parentNode" # "remove" $ ()
                    toggleSaveSetButton
                _                  -> pure ()
    setNameInput <- M.getElementById $ ms "setNameInput"
    M.addEventListener setNameInput (ms "input") $ const toggleSaveSetButton
    pure DoNothing
  where
    addTranslate :: JSVal -> M.JSM ()
    addTranslate event = do
        unitForm <- event ! "target" ! "parentNode"
        translate <- unitForm # "querySelector" $ [ ".Translate:last-child" ]
        translate' <- translate # "cloneNode" $ True
        (translate' # "querySelector" $ [ ".FormField" ]) <# "value" $ ""
        void $ translate # "after" $ translate'

    deleteTranslate :: JSVal -> M.JSM ()
    deleteTranslate event = do
        translate <- event ! "target" ! "parentNode"
        translates' <- translate ! "parentNode" ! "children"
        translatesCount <- valToNumber $ translates' ! "length"
        void $ if translatesCount == 1
            then void
                $ translate ! "parentNode" ! "parentNode" ! "parentNode" # "remove" $ ()
            else void $ translate # "remove" $ ()
    toggleSaveSetButton = do
        setName <- valToStr =<< (M.getElementById $ ms "setNameInput") ! "value"
        units' <- formData (ms "edit-unit-form") False :: M.JSM [Unit]
        saveSetButton <- M.getElementById $ ms "saveSetButton"
        if (model ^? sets.ix setIx ^. non (Set (ms "") Nothing))
                == Set setName (if null units' then Nothing else Just units')
            then saveSetButton ! "style" <# "display" $ "none"
            else saveSetButton ! "style" <# "display" $ "inline-block"
updateModel (SaveSet setIx)               model = model M.<# do
    saveSetButton <- M.getElementById $ ms "saveSetButton"
    saveSetButton ! "style" <# "display" $ [ "none" ] 
    setName <- valToStr =<< (M.getElementById $ ms "setNameInput") ! "value"
    units' <- formData (ms "edit-unit-form") False :: M.JSM [Unit]
    pure . UpdateSets (Just setIx) . Set setName
        $ if null units' then Nothing else Just units'
updateModel SaveSets                      model = model M.<#
    (M.setLocalStorage (ms "sets") (model ^. sets) >> pure DoNothing)
updateModel (UpdateSets (Just setIx) set) model =
    (model&sets.ix setIx .~ set) M.<# pure SaveSets
updateModel (UpdateSets Nothing      set) model =
    (model&sets %~ (++ [ set ])) M.<# pure SaveSets
updateModel (UpdateUnits setIx unit)      model =
    (model&sets.ix setIx.units.non [] %~ (++ [ unit ])) M.<# pure SaveSets
