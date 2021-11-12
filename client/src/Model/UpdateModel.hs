{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Model.UpdateModel
    ( updateModel
    ) where

import Control.Concurrent (threadDelay)
import Control.Lens ((%~), (&), (+~), (-~), (.~), (^.), (^..), (^?), _1, _2)
import Control.Lens.Combinators (_head, ifiltered, ix, non, each, filtered, to, withIndex)
import Control.Lens.Extras (is)
import Control.Lens.Prism (_Just, _Left, _Right)
import Control.Lens.TH (makeFieldsNoPrefix)
import Control.Lens.Traversal (traversed)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.Maybe (listToMaybe)
import Miso.String (fromMisoString, ms)
import System.Environment (getEnv)

import Utils (formData, listedStep, pagesCount, set', setResultsIsDone)
import Views.Smart.Router.Utils (goSet)

import qualified Language.Javascript.JSaddle as LJJ
import qualified Miso as M

import qualified Common as C
import qualified Model.Model as MM
import qualified Model.Action as MA


makeFieldsNoPrefix ''C.LanguageMemorizer
makeFieldsNoPrefix ''C.Set
makeFieldsNoPrefix ''C.SharedSet
makeFieldsNoPrefix ''C.Unit
makeFieldsNoPrefix ''MM.EditedSet
makeFieldsNoPrefix ''MM.LiteLanguageMemorizer
makeFieldsNoPrefix ''MM.LiteSet
makeFieldsNoPrefix ''MM.Memorizing
makeFieldsNoPrefix ''MM.Model
makeFieldsNoPrefix ''MM.Pages
makeFieldsNoPrefix ''MM.Pagination
makeFieldsNoPrefix ''MM.SetResult
makeFieldsNoPrefix ''MM.SetResultStep
makeFieldsNoPrefix ''MM.SetsPagination
makeFieldsNoPrefix ''MM.Settings

updateModel :: MA.Action -> MM.Model -> M.Effect MA.Action MM.Model
updateModel MA.AddSet                                                       model =
    model M.<# do
        (set'':_) <- formData (ms "add-set-form") True :: M.JSM [C.Set]
        pure $ MA.UpdateSets False (Just MA.MyLocal) Nothing $ Left set''
updateModel (MA.AddTranslate unitIx')                                       model = M.noEff
    $ model&sets.ix (model ^. activeSetIx)._Left.units._Just.ix unitIx'.translates
        %~ (++ [ "" ])
updateModel MA.AddUnit                                                      model =
    model M.<# (pure . MA.UpdateUnits (model ^. activeSetIx) $ C.Unit "" [ "" ])
updateModel (MA.ChangeUri uri')                                             model =
    (model&menuIsVisible .~ False) M.<# (M.pushURI uri' >> pure MA.DoNothing)
updateModel (MA.CheckAnswer MA.Text' text')                                 model =
    (model&memorizing.answer .~ text')
        M.<# (pure
            $ if model
                ^? sets.ix (memorizing' ^. setIx).to set'
                    .units._Just.ix (memorizing' ^. unitIx).text
                ^. non "".to ((== text') . ms)
            then MA.SelectRandomMemorizingUnit
            else MA.DoNothing)
  where
    memorizing' = model ^. memorizing
updateModel (MA.CheckAnswer MA.Translates translate)                        model =
    (model&memorizing.answer .~ translate)
        M.<# (pure . maybe MA.DoNothing (const MA.SelectRandomMemorizingUnit)
            $ model
                ^? sets.ix (model ^. memorizing.setIx).to set'
                    .units._Just.ix (model ^. memorizing.unitIx)
                    .translates.each.to ms.filtered (== translate))
updateModel (MA.DeleteSet setIx')                                           model = (model
    &sets %~ (^.. traversed.ifiltered (\setIx'' _ -> setIx'' /= setIx'))
    &pagination.sets.myLocal.count -~  isLastPageElem
    &pagination.sets.myLocal.current -~ isLastPageElem
    &sets %~ (^.. traversed.ifiltered (\setIx'' _ -> setIx'' /= setIx'))
    ) M.<# pure MA.SaveSets
  where
    isLastPageElem =
        (if model ^. sets.to length
            == (model ^. pagination.sets.myLocal.count - 1)
                * model ^. settings.setsPageCount.to read + 1
        then 1
        else 0)
updateModel (MA.DeleteTranslate unitIx' transIx)                            model = M.noEff
    $ model&sets.ix (model ^. activeSetIx)._Left.units._Just.ix unitIx'.translates
        %~ (^.. traversed.ifiltered (\transIx' _ -> transIx' /= transIx))
updateModel (MA.DeleteUnit unitIx')                                         model = M.batchEff
    (model
        &pagination.units.ix activeSetIx'.count -~ isLastPageElem
        &pagination.units.ix activeSetIx'.current -~ isLastPageElem
        &sets.ix activeSetIx'._Left.units._Just %~ newUnits
        &sets.ix activeSetIx'._Right._Left.set.units._Just %~ newUnits
    )
    [ pure MA.SaveSets
    , pure . MA.UpdatePagination . MA.Part . MA.Units $ model ^. activeSetIx
    ]
  where
    isLastPageElem =
        (if model ^? sets.ix activeSetIx'.to set'.units._Just.to length ^. non (-1)
            == (model ^? pagination.units.ix activeSetIx'.count ^. non 0.to (subtract 1))
                * model ^. settings.unitsPageCount.to read + 1
        then 1
        else 0)
    newUnits       = (^.. traversed.ifiltered (\unitIx'' _ -> unitIx'' /= unitIx'))
    activeSetIx'   = model ^. activeSetIx
updateModel MA.DoNothing                                                    model =
    M.noEff model
updateModel (MA.DownloadSharedSet sharedSetId)                              model =
    model M.<# do
        getSharedSetUrl <- liftIO $ getEnv "api_server_get_shared_set_url"
        respPromise <- LJJ.jsg1 "fetch" $ getSharedSetUrl ++ "/" ++ show sharedSetId
        _ <- respPromise LJJ.# "then" $ LJJ.fun $ \_ _ [ resp ] -> do
            _ <- (resp LJJ.# "text" $ ()) LJJ.# "then"
                $ LJJ.fun $ \_ _ [ jsonJsValSharedSet ] -> do
                    jsonSharedSet <- fromMisoString <$> LJJ.valToStr jsonJsValSharedSet
                    _ <- LJJ.jsg "localStorage" LJJ.# "setItem"
                        $ [ "tmpSharedSet", jsonSharedSet ]
                    pure ()
            pure ()
        liftIO $ threadDelay 1000000
        Right sharedSet <- M.getLocalStorage $ ms "tmpSharedSet"
            :: M.JSM (Either String C.SharedSet)
        M.removeLocalStorage $ ms "tmpSharedSet"
        if is (_Just._Left) $ sets' ^? ix (sets' ^. to length - 1)._Right._Right
            then
                pure . MA.UpdateSets True Nothing (Just lastSetIx) . Right . Right . Left
                    $ MM.BeingDownloadedSet sharedSet
            else
                pure . MA.UpdateSets True Nothing Nothing . Right . Right . Left
                    $ MM.BeingDownloadedSet sharedSet
  where
    lastSetIx = sets' ^. to length - 1
    sets'     = model ^. sets
updateModel (MA.EditSet MA.Name _ name')                                    model =
    M.noEff $ model&editedSet.name .~ name'
updateModel (MA.EditSet editedUnitPart unitIx' editedUnitPartVal)           model =
    M.noEff
        $ newModel
            &(if newEditedUnit
                == model
                    ^? sets.ix (model ^. activeSetIx).to set'.units._Just.ix unitIx'
                    ^. non zeroUnit
                then editedSet.ixedUnits %~ (^.. traversed.filtered
                    (\(unitIx'', _) -> unitIx'' /= unitIx'))
                else editedSet.ixedUnits %~ (^.. traversed.to (\ixedEditedUnit ->
                    if ixedEditedUnit ^. _1 == unitIx'
                    then ixedEditedUnit&_2 .~ newEditedUnit
                    else ixedEditedUnit)))
  where
    editedUnit    = newModel ^.. editedSet.ixedUnits.traversed.filtered (\(unitIx'', _) ->
        unitIx'' == unitIx') ^? to listToMaybe._Just._2 ^. non zeroUnit
    newEditedUnit = case editedUnitPart of
        MA.UnitText              -> editedUnit&text .~ fromMisoString editedUnitPartVal
        MA.UnitTranslate transIx ->
            editedUnit&translates.ix transIx .~ fromMisoString editedUnitPartVal
        _                        -> editedUnit
    newModel      =
        (if null $ model ^.. editedSet.ixedUnits.traversed.filtered
            (\(unitIx'', _) -> unitIx'' == unitIx')
        then model&editedSet.ixedUnits %~ (++
            [
                ( unitIx'
                , model
                    ^? sets.ix (model ^. activeSetIx).to set'.units._Just.ix unitIx'
                    ^. non zeroUnit
                )
            ])
        else model)
    zeroUnit      = C.Unit "" []
updateModel MA.FailMemorizingStep                                           model = (model
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
updateModel (MA.HandleUri uri')                                             model =
    M.noEff $ model&uri .~ uri'
updateModel (MA.RefreshSet setIx')                                          model = (model
    &editedSet.ixedUnits .~ []
    &editedSet.name .~ model ^? sets.ix setIx'.to set'.name ^. non "".to ms
    &activeSetIx .~ setIx'
    ) M.<# pure MA.DoNothing
updateModel MA.RepeatMemorizing                                             model = (model
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
            (\setIx' set''->
                elem (show setIx') (model ^? settings.activeSetIxs._Just ^. non [])
                    && (not . null $ set'' ^? to set'.units._Just ^. non []))
            .withIndex)
        ^.. traversed.to (\(setIx', set'') ->
            MM.LiteSet setIx'
                $ [ 0..set'' ^. to set'.units.non [].to ((subtract 1) . length) ])
    setResults    = activeSetIxs' ^.. _Just.each.to ((`MM.SetResult` []) . read)
updateModel MA.SaveSet                                                      model = (model
    &editedSet.ixedUnits .~ []
    &sets.ix activeSetIx'._Left
        .~ C.Set (model ^. editedSet.name.to fromMisoString) (Just newUnits)
    &sets.ix activeSetIx'._Right._Left.set
        .~ C.Set (model ^. editedSet.name.to fromMisoString) (Just newUnits)
    ) M.<# pure MA.SaveSets
  where
    newUnits     = model
        ^.. sets.ix activeSetIx'.to set'.units._Just.traversed.withIndex.to
            (\(editedUnitIx, editedUnit) -> (model ^.. editedSet.ixedUnits.traversed.filtered
                (\(editedUnitIx', _) -> editedUnitIx' == editedUnitIx))
        ^? _head . _2
        ^. non editedUnit)
    activeSetIx' = model ^. activeSetIx
updateModel MA.SaveSets                                                     model =
    model M.<#
        (M.setLocalStorage (ms "sets") (model ^. sets) >> pure MA.DoNothing)
updateModel MA.SaveSettings                                                 model =
    model M.<# do
        (settings':_) <- formData (ms "edit-settings-form") False :: M.JSM [MM.Settings]
        M.setLocalStorage (ms "settings") settings'
        pure $ MA.UpdateSettings settings'
updateModel MA.SelectRandomMemorizingUnit                                   model =
    model M.<# do
        [ randomNum, randomNum1, randomNum2 ] <- replicateM 3
            $ LJJ.valToNumber =<< (LJJ.jsg "Math" LJJ.# "random" $ ())
        let
            liteSet      = model
                ^? memorizing.liteSets.ix liteSetIx' ^. non (MM.LiteSet (-1) [])
            liteSetIx'   = randomIx randomNum $ model ^. memorizing.liteSets.to length
            liteUnitIx   = randomIx randomNum1
                $ model ^? memorizing.liteSets.ix liteSetIx'.unitIxs ^. non [].to length
            translateIx' = randomIx randomNum2
                $ model
                    ^? sets.ix (liteSet ^. setIx)
                        .to set'.units._Just.ix unitIx'.translates
                    ^. non [].to length
            unitIx'      = liteSet ^? unitIxs.ix liteUnitIx ^. non (-1)
        pure $ MA.UpdateMemorizing liteSetIx' liteUnitIx translateIx'
  where
    randomIx :: Double -> Int -> Int
    randomIx randomNum' length' = floor $ randomNum' * fromIntegral length'
updateModel MA.SetDownloadedSet                                             model =
    M.batchEff
        (model&sets.ix (model ^. sets.to length - 1)._Right._Right
            %~ (\(Left (MM.BeingDownloadedSet sharedSet)) ->
                Right $ MM.DownloadedSet sharedSet))
        [ pure MA.SaveSets, pure . MA.UpdatePagination . MA.Part . MA.Sets $ MA.Downloaded ]
updateModel (MA.SetLanguageMemorizerName langMemorizerName')                model =
    M.noEff $ model&langMemorizerName .~ Just langMemorizerName'
updateModel (MA.SetLiteSharedSets liteSharedSets')                          model =
    (model&liteSharedSets .~ liteSharedSets')
        M.<# pure (MA.UpdatePagination . MA.Part $ MA.Sets MA.TheirShared)
updateModel (MA.SetNewSharedSetId sharedSetIx newSharedSetId)               model =
    (model&sets.ix sharedSetIx._Right._Left.Model.UpdateModel.id .~ newSharedSetId)
        M.<# pure MA.SaveSets
updateModel (MA.ShareSet setIx')                                            model =
    model M.<# do
        fetchOptions <- LJJ.create
        fetchOptions LJJ.<# "body"
            $ ms . encode $ model ^? sets.ix setIx'._Left ^. non (C.Set "" Nothing)
        jsValAuthToken <- LJJ.jsg "localStorage" LJJ.# "getItem" $ [ "authToken" ]
        authToken <- fromMisoString <$> LJJ.valToStr jsValAuthToken
        fetchHeaders <- LJJ.create
        fetchHeaders LJJ.<# "authorization" $ "Bearer " ++ authToken
        fetchHeaders LJJ.<# "content-type" $ "application/json"
        fetchOptions LJJ.<# "headers" $ fetchHeaders
        fetchOptions LJJ.<# "method" $ "POST"
        shareSetUrl <- liftIO $ getEnv "api_server_share_set_url"
        respPromise <- LJJ.jsg2 "fetch" shareSetUrl fetchOptions
        _ <- respPromise LJJ.# "then" $ LJJ.fun $ \_ _ [ resp ] -> do
            textPromise <- resp LJJ.# "text" $ ()
            _ <- textPromise LJJ.# "then" $ LJJ.fun $ \_ _ [ jsValSharedSetId ] -> do
                sharedSetId <- fromMisoString <$> LJJ.valToStr jsValSharedSetId
                _ <- LJJ.jsg "localStorage" LJJ.# "setItem"
                    $ [ "tmpSharedSetId", sharedSetId ]
                pure ()
            pure ()
        liftIO $ threadDelay 1000000
        jsValTmpSharedSetId <- LJJ.jsg "localStorage" LJJ.# "getItem"
            $ [ "tmpSharedSetId" ]
        sharedSetId <- LJJ.valToNumber jsValTmpSharedSetId
        M.removeLocalStorage $ ms "tmpSharedSetId"
        pure
            . MA.UpdateSets False (Just MA.MyShared) (Just setIx')
            . Right
            . Left
            . C.SharedSet (round sharedSetId)
            $ model ^? sets.ix setIx'._Left ^. non (C.Set "" Nothing)
updateModel (MA.ShowAnswer MA.Text')                                        model = (model
    &memorizing.answer .~ model
            ^? sets.ix (memorizing' ^. setIx)
                .to set'.units._Just.ix (memorizing' ^. unitIx).text.to ms
            ^. non (ms ""))
        M.<# pure MA.FailMemorizingStep
  where
    memorizing' = model ^. memorizing
updateModel (MA.ShowAnswer MA.Translates)                                   model = (model
    &memorizing.answer .~ model
            ^? sets.ix (memorizing' ^. setIx)
                .to set'.units._Just.ix (memorizing' ^. unitIx)
                .translates.ix (memorizing' ^. translateIx)
            ^. non "".to ms)
        M.<# pure MA.FailMemorizingStep
  where
    memorizing' = model ^. memorizing
updateModel MA.SignIn                                                       model =
    model M.<# do
        (langMemorizer':_) <- formData (ms "sign-in-form") True
            :: M.JSM [C.LanguageMemorizer]
        fetchOptions <- LJJ.create
        fetchOptions LJJ.<# "body" $ ms $ encode langMemorizer'
        fetchHeaders <- LJJ.create
        fetchHeaders LJJ.<# "content-type" $ "application/json"
        fetchOptions LJJ.<# "headers" $ fetchHeaders
        fetchOptions LJJ.<# "method" $ "POST"
        signInUrl <- liftIO $ getEnv "api_server_sign_in_url"
        respPromise <- LJJ.jsg2 "fetch" signInUrl fetchOptions
        _ <- respPromise LJJ.# "then" $ LJJ.fun $ \_ _ [ resp ] -> do
            _ <- (resp LJJ.# "json" $ ()) LJJ.# "then"
                $ LJJ.fun $ \_ _ [ jsValNameAndAuthToken ] -> do
                    langMemorizerIsSignedIn <- LJJ.valToBool $ resp LJJ.! "ok"
                    if langMemorizerIsSignedIn
                        then do
                            jsValAuthToken <- jsValNameAndAuthToken LJJ.! "1"
                            authToken <- fromMisoString <$> LJJ.valToStr jsValAuthToken
                            _ <- LJJ.jsg "localStorage" LJJ.# "setItem"
                                $ [ "authToken", authToken ]
                            jsValName <- jsValNameAndAuthToken LJJ.! "0"
                            name' <- fromMisoString <$> LJJ.valToStr jsValName
                            _ <- LJJ.jsg "localStorage" LJJ.# "setItem"
                                $ [ "langMemorizerName", name' ]
                            pure ()
                        else pure ()
            pure ()
        liftIO $ threadDelay 1000000
        jsValLangMemorizerName <- LJJ.jsg "localStorage" LJJ.# "getItem"
            $ [ "langMemorizerName" ]
        langMemorizerName' <- LJJ.valToStr jsValLangMemorizerName
        jsValLangMemorizerNameIsNull <- LJJ.valIsNull jsValLangMemorizerName
        pure
            $ if jsValLangMemorizerNameIsNull
            then MA.DoNothing
            else MA.SetLanguageMemorizerName langMemorizerName' 
updateModel MA.SignOut                                                      model =
    (model&langMemorizerName .~ Nothing)
        M.<# do
            M.removeLocalStorage $ ms "authToken"
            M.removeLocalStorage $ ms "langMemorizerName"
            pure MA.DoNothing
updateModel MA.SignUp                                                       model =
    model M.<# do
        (langMemorizer':_) <- formData (ms "sign-up-form") True
            :: M.JSM [C.LanguageMemorizer]
        fetchOptions <- LJJ.create
        fetchOptions LJJ.<# "body" $ ms $ encode langMemorizer'
        fetchHeaders <- LJJ.create
        fetchHeaders LJJ.<# "content-type" $ "application/json"
        fetchOptions LJJ.<# "headers" $ fetchHeaders
        fetchOptions LJJ.<# "method" $ "POST"
        signUpUrl <- liftIO $ getEnv "api_server_sign_up_url"
        _ <- LJJ.jsg2 "fetch" signUpUrl fetchOptions
        pure MA.DoNothing
updateModel (MA.SwitchActiveSetsType MA.Downloaded)                         model =
    M.noEff $
        model&activeSetsType .~ MM.Downloaded
updateModel (MA.SwitchActiveSetsType MA.MyLocal)                            model =
    M.noEff $
        model&activeSetsType .~ MM.Local
updateModel (MA.SwitchActiveSetsType MA.MyShared)                           model =
    M.noEff $
        model&activeSetsType .~ MM.MyShared
updateModel (MA.SwitchActiveSetsType MA.TheirShared)                        model = (model
    &activeSetsType .~ MM.Shared
    ) M.<# do
        getSharedSetsIdsAndNamesUrl <- liftIO
            $ getEnv "api_server_get_shared_sets_ids_and_names_url"
        respPromise <- LJJ.jsg1 "fetch" getSharedSetsIdsAndNamesUrl
        _ <- respPromise LJJ.# "then" $ LJJ.fun $ \_ _ [ resp ] -> do
            _ <- (resp LJJ.# "text" $ ()) LJJ.# "then"
                $ LJJ.fun $ \_ _ [ jsonJsValSharedSetsIdsAndNames ] -> do
                    jsonSharedSetsIdsAndNames <- fromMisoString
                        <$> LJJ.valToStr jsonJsValSharedSetsIdsAndNames
                    _ <- LJJ.jsg "localStorage" LJJ.# "setItem"
                        $ [ "tmpSharedSetsIdsAndNames", jsonSharedSetsIdsAndNames ]
                    pure ()
            pure ()
        liftIO $ threadDelay 1000000
        Right liteSharedSets' <- M.getLocalStorage $ ms "tmpSharedSetsIdsAndNames"
            :: M.JSM (Either String [C.LiteSharedSet])
        M.removeLocalStorage $ ms "tmpSharedSetsIdsAndNames"
        pure $ MA.SetLiteSharedSets liteSharedSets'
updateModel (MA.SwitchPage MA.First (MA.Sets MA.Downloaded))                model =
    M.noEff
        $ model&pagination.sets.downloaded.current .~ 0
updateModel (MA.SwitchPage (MA.Jump page) (MA.Sets MA.Downloaded))          model =
    M.noEff
        $ model&pagination.sets.downloaded.current .~ page
updateModel (MA.SwitchPage MA.Last (MA.Sets MA.Downloaded))                 model =
    M.noEff
        $ model&pagination.sets.downloaded.current
            .~ model ^. pagination.sets.downloaded.count.to (subtract 1)
updateModel (MA.SwitchPage MA.Next (MA.Sets MA.Downloaded))                 model =
    M.noEff $
        if model ^. pagination.sets.downloaded.current
            < model ^. pagination.sets.downloaded.count.to (subtract 1)
        then model&pagination.sets.downloaded.current +~ 1
        else model
updateModel (MA.SwitchPage MA.Previous (MA.Sets MA.Downloaded))             model =
    M.noEff $
        if model ^. pagination.sets.downloaded.current == 0
        then model
        else model&pagination.sets.downloaded.current -~ 1
updateModel (MA.SwitchPage MA.First (MA.Sets MA.MyLocal))                   model =
    M.noEff
        $ model&pagination.sets.myLocal.current .~ 0
updateModel (MA.SwitchPage (MA.Jump page) (MA.Sets MA.MyLocal))             model =
    M.noEff
        $ model&pagination.sets.myLocal.current .~ page
updateModel (MA.SwitchPage MA.Last (MA.Sets MA.MyLocal))                    model =
    M.noEff
        $ model&pagination.sets.myLocal.current
            .~ model ^. pagination.sets.myLocal.count.to (subtract 1)
updateModel (MA.SwitchPage MA.Next (MA.Sets MA.MyLocal))                    model =
    M.noEff $
        if model ^. pagination.sets.myLocal.current
            < model ^. pagination.sets.myLocal.count.to (subtract 1)
        then model&pagination.sets.myLocal.current +~ 1
        else model
updateModel (MA.SwitchPage MA.Previous (MA.Sets MA.MyLocal))                model =
    M.noEff $
        if model ^. pagination.sets.myLocal.current == 0
        then model
        else model&pagination.sets.myLocal.current -~ 1
updateModel (MA.SwitchPage MA.First (MA.Sets MA.MyShared))                  model =
    M.noEff
        $ model&pagination.sets.myShared.current .~ 0
updateModel (MA.SwitchPage (MA.Jump page) (MA.Sets MA.MyShared))            model =
    M.noEff
        $ model&pagination.sets.myShared.current .~ page
updateModel (MA.SwitchPage MA.Last (MA.Sets MA.MyShared))                   model =
    M.noEff
        $ model&pagination.sets.myShared.current
            .~ model ^. pagination.sets.myShared.count.to (subtract 1)
updateModel (MA.SwitchPage MA.Next (MA.Sets MA.MyShared))                   model =
    M.noEff $
        if model ^. pagination.sets.myShared.current
            < model ^. pagination.sets.myShared.count.to (subtract 1)
        then model&pagination.sets.myShared.current +~ 1
        else model
updateModel (MA.SwitchPage MA.Previous (MA.Sets MA.MyShared))               model =
    M.noEff $
        if model ^. pagination.sets.myShared.current == 0
        then model
        else model&pagination.sets.myShared.current -~ 1
updateModel (MA.SwitchPage MA.First (MA.Sets MA.TheirShared))               model =
    M.noEff
        $ model&pagination.sets.theirShared.current .~ 0
updateModel (MA.SwitchPage (MA.Jump page) (MA.Sets MA.TheirShared))         model =
    M.noEff
        $ model&pagination.sets.theirShared.current .~ page
updateModel (MA.SwitchPage MA.Last (MA.Sets MA.TheirShared))                model =
    M.noEff
        $ model&pagination.sets.theirShared.current
            .~ model ^. pagination.sets.theirShared.count.to (subtract 1)
updateModel (MA.SwitchPage MA.Next (MA.Sets MA.TheirShared))                model =
    M.noEff $
        if model ^. pagination.sets.theirShared.current
            < model ^. pagination.sets.theirShared.count.to (subtract 1)
        then model&pagination.sets.theirShared.current +~ 1
        else model
updateModel (MA.SwitchPage MA.Previous (MA.Sets MA.TheirShared))            model =
    M.noEff $
        if model ^. pagination.sets.theirShared.current == 0
        then model
        else model&pagination.sets.theirShared.current -~ 1
updateModel (MA.SwitchPage MA.First MA.Statistics)                          model =
    M.noEff $ model&pagination.statistics.current .~ 0
updateModel (MA.SwitchPage (MA.Jump page) MA.Statistics)                    model =
    M.noEff $ model&pagination.statistics.current .~ page
updateModel (MA.SwitchPage MA.Last MA.Statistics)                           model =
    M.noEff
        $ model
            &pagination.statistics.current .~ model ^. pagination.sets.downloaded.count.to
                (subtract 1)
updateModel (MA.SwitchPage MA.Next MA.Statistics)                           model =
    M.noEff $ if model ^. pagination.statistics.current
        < model ^. pagination.statistics.count.to (subtract 1)
    then model&pagination.statistics.current +~ 1
    else model
updateModel (MA.SwitchPage MA.Previous MA.Statistics)                       model =
    M.noEff $ if model ^. pagination.statistics.current == 0
    then model
    else model&pagination.statistics.current -~ 1
updateModel (MA.SwitchPage MA.First (MA.Units setIx'))                      model =
    (model&pagination.units.ix setIx'.current .~ 0) M.<# pure (MA.RefreshSet setIx')
updateModel (MA.SwitchPage (MA.Jump page) (MA.Units setIx'))                model =
    (model&pagination.units.ix setIx'.current .~ page) M.<# pure (MA.RefreshSet setIx')
updateModel (MA.SwitchPage MA.Last (MA.Units setIx'))                       model =
    (model&pagination.units.ix setIx'.current .~ (model
        ^? pagination.units.ix setIx'.count
        ^. non 1.to (subtract 1))) M.<# pure (MA.RefreshSet setIx')
updateModel (MA.SwitchPage MA.Next (MA.Units setIx'))                       model =
    (if model ^? pagination.units.ix setIx'.current ^. non (-1)
        <
        (model ^? pagination.units.ix setIx'.count ^. non 1.to (subtract 1))
    then model&pagination.units.ix setIx'.current +~ 1
    else model) M.<# pure (MA.RefreshSet setIx')
updateModel (MA.SwitchPage MA.Previous (MA.Units setIx'))                   model =
    (if model ^? pagination.units.ix setIx'.current ^. non (-1) == 0
    then model
    else model&pagination.units.ix setIx'.current -~ 1) M.<# pure (MA.RefreshSet setIx')
updateModel MA.ToggleMenuVisibility                                         model =
    (model&menuIsVisible %~ not) M.<# do
        _ <- M.getBody LJJ.! "classList" LJJ.# "toggle" $ [ "covered" ]
        pure MA.DoNothing
updateModel (MA.UnshareSet sharedSetIx)                                     model = model M.<# do
    fetchOptions <- LJJ.create
    jsValAuthToken <- LJJ.jsg "localStorage" LJJ.# "getItem" $ [ "authToken" ]
    authToken <- fromMisoString <$> LJJ.valToStr jsValAuthToken
    fetchHeaders <- LJJ.create
    fetchHeaders LJJ.<# "authorization" $ "Bearer " ++ authToken
    fetchOptions LJJ.<# "headers" $ fetchHeaders
    fetchOptions LJJ.<# "method" $ "DELETE"
    unshareSetUrl <- liftIO $ getEnv "api_server_unshare_set_url"
    _ <- LJJ.jsg2
        "fetch"
        (unshareSetUrl
            ++ "/"
            ++ model
                ^? sets.ix sharedSetIx._Right._Left.Model.UpdateModel.id
                ^. non (-1) ^. to show)
        fetchOptions
    pure . MA.UpdateSets False (Just MA.MyLocal) (Just sharedSetIx) . Left
        $ model ^? sets.ix sharedSetIx._Right._Left.set ^. non (C.Set "" Nothing)
updateModel (MA.UpdateMemorizing liteSetIx' tmpUnitIx' translateIx')        model = (model
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
updateModel (MA.UpdatePagination (MA.Part (MA.Sets MA.Downloaded)))         model =
    M.noEff
        $ model
            &pagination.sets.downloaded.count
                .~ (pagesCount (model ^. settings.setsPageCount.to read)
                    $ (model ^.. sets.traversed._Right._Right._Right ^. to length))
            &pagination.units %~ (++ [ MM.Pages 0 0 ])
updateModel (MA.UpdatePagination (MA.Part (MA.Sets MA.MyLocal)))            model =
    M.noEff
        $ model
            &pagination.sets.myLocal.count
                .~ (pagesCount (model ^. settings.setsPageCount.to read)
                    $ (model ^.. sets.traversed._Left ^. to length))
            &pagination.units %~ (++ [ MM.Pages 0 0 ])
updateModel (MA.UpdatePagination (MA.Part (MA.Sets MA.MyShared)))           model =
    M.noEff
        $ model
            &pagination.sets.myShared.count
                .~ (pagesCount (model ^. settings.setsPageCount.to read)
                    $ (model ^.. sets.traversed._Right._Left ^. to length))
            &pagination.units %~ (++ [ MM.Pages 0 0 ])
updateModel (MA.UpdatePagination (MA.Part (MA.Sets MA.TheirShared)))        model =
    M.noEff
        $ model
            &pagination.sets.theirShared.count
                .~ (pagesCount (model ^. settings.setsPageCount.to read)
                    $ (model ^. liteSharedSets.to length))
            &pagination.units %~ (++ [ MM.Pages 0 0 ])
updateModel (MA.UpdatePagination (MA.Part MA.Statistics))                   model =
    M.noEff
        $ model
            &pagination.statistics.count .~ (pagesCount
                (model ^. settings.statisticsPageCount.to read)
                $ (model ^. statistics.to length))
updateModel (MA.UpdatePagination (MA.Part (MA.Units setIx')))               model =
    M.noEff
        $ model&pagination.units.ix setIx'.count .~ (pagesCount
            (model ^. settings.unitsPageCount.to read)
            $ (model ^? sets.ix setIx'.to set'.units._Just.to length) ^. non (-1))
updateModel (MA.UpdatePagination MA.Whole)                                  model =
    (model&pagination.units.traversed.withIndex %~ (\setPages@(setIx', _) ->
            setPages&_2.count .~ (pagesCount (model ^. settings.unitsPageCount.to read)
                $ (model ^? sets.ix setIx'.to set'.units._Just.to length) ^. non (-1))))
        M.<# pure (MA.UpdatePagination $ MA.Part (MA.Sets MA.Downloaded))
updateModel (MA.UpdateSets False (Just setsType) (Just setIx') set'')       model =
    M.batchEff
        (model&sets.ix setIx' .~ set'')
        [ pure MA.SaveSets, pure . MA.UpdatePagination . MA.Part $ MA.Sets setsType
        ]
updateModel (MA.UpdateSets False (Just setsType) Nothing       set'')       model =
    M.batchEff
        (model&sets %~ (++ [ set'' ]))
        [ pure MA.SaveSets, pure . MA.UpdatePagination . MA.Part $ MA.Sets setsType
        ]
updateModel (MA.UpdateSets True Nothing (Just setIx') set'')                model =
    (model&sets.ix setIx' .~ set'') M.<# pure (goSet $ model ^. sets.to length - 1)
updateModel (MA.UpdateSets True Nothing Nothing       set'')                model =
    (model&sets %~ (++ [ set'' ])) M.<# pure (goSet $ model ^. sets.to length)
updateModel (MA.UpdateSets _    _       _             _)                    model =
    model M.<# pure MA.DoNothing
updateModel (MA.UpdateSettings settings')                                   model =
    M.batchEff
        (model&settings .~ settings')
        [ pure MA.RepeatMemorizing, pure $ MA.UpdatePagination MA.Whole ]
updateModel (MA.UpdateSharedSet sharedSetIx)                                model = model M.<# do
    fetchOptions <- LJJ.create
    fetchOptions LJJ.<# "body"
        $ ms . encode
            $ model ^? sets.ix sharedSetIx._Right._Left ^. non (C.SharedSet (-1)
                $ C.Set "" Nothing)
    jsValAuthToken <- LJJ.jsg "localStorage" LJJ.# "getItem" $ [ "authToken" ]
    authToken <- fromMisoString <$> LJJ.valToStr jsValAuthToken
    fetchHeaders <- LJJ.create
    fetchHeaders LJJ.<# "authorization" $ "Bearer " ++ authToken
    fetchHeaders LJJ.<# "content-type" $ "application/json"
    fetchOptions LJJ.<# "headers" $ fetchHeaders
    fetchOptions LJJ.<# "method" $ "POST"
    updateSharedSetUrl <- liftIO $ getEnv "api_server_update_shared_set_url"
    respPromise <- LJJ.jsg2 "fetch" updateSharedSetUrl fetchOptions
    _ <- respPromise LJJ.# "then" $ LJJ.fun $ \_ _ [ resp ] -> do
        newSharedSetIdPromise <- resp LJJ.# "text" $ ()
        _ <- newSharedSetIdPromise LJJ.# "then"
            $ LJJ.fun $ \_ _ [ newSharedSetId ] ->
                M.setLocalStorage (ms "tmpNewSharedSetId") =<< LJJ.valToStr newSharedSetId
        pure ()
    liftIO $ threadDelay 1000000
    eitherNewSharedSetId <- M.getLocalStorage $ ms "tmpNewSharedSetId"
        :: M.JSM (Either String String)
    M.removeLocalStorage $ ms "tmpNewSharedSetId"
    pure
        $ either
            (const MA.DoNothing)
            ((MA.SetNewSharedSetId sharedSetIx) . read)
            eitherNewSharedSetId
updateModel (MA.UpdateUnits setIx' unit)                                    model = M.batchEff
    (model
        &sets.ix setIx'._Left.units.non [] %~ (++ [ unit ])
        &sets.ix setIx'._Right._Left.set.units.non [] %~ (++ [ unit ]))
    [ pure MA.SaveSets
    , pure . MA.UpdatePagination . MA.Part $ MA.Units setIx'
    , if model ^. settings.activeSetIxs.non [].to ((elem setIx') . map read)
        then pure MA.RepeatMemorizing
        else pure MA.DoNothing
    ]
