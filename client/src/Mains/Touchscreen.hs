{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Touchscreen
    ( main
    ) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle.Warp (run)
import System.Directory (makeAbsolute)
import Text.Sass.Compilation (StringResult, compileFile)
import Text.Sass.Options (defaultSassOptions)
#endif
import Control.Lens ((^.), (^..), (^?))
import Control.Lens.Combinators (_Left, _Right, non, to)
import Control.Lens.Extras (is)
import Control.Lens.TH (makeFieldsNoPrefix)
import Language.Javascript.JSaddle ((!), (#), (<#), jsg, valIsNull, valToStr)
import Miso.String (ms)
import System.Environment (getEnv)

import Common (Set (), SharedSet ())
import Model.UpdateModel (updateModel)
import Utils (SetIx (), pagesCount, set')
import Views.Dumb.Providing.Root.Touchscreen (root)

import qualified Control.Lens.Combinators as CLC
import qualified Miso as M

import qualified Model.Action as MA
import qualified Model.Model as MM


makeFieldsNoPrefix ''MM.LiteSet
makeFieldsNoPrefix ''MM.Settings
makeFieldsNoPrefix ''Set

#ifndef __GHCJS__
runApp :: M.JSM () -> IO ()
runApp app = do
    sassMainPath <- makeAbsolute "client/src/Views/Dumb/Providing/Root/Touchscreen.sass"
    Right css <- compileFile sassMainPath defaultSassOptions :: StringResult
    port <- getEnv "touchscreen_client_dev_www_server_port"
    run (read port) $ do
        let meta = "<meta name='viewport' content='width=device-width, initial-scale=1'>"
        M.getDoc ! "head" <# "innerHTML" $ meta ++ "<style>" ++ css ++ "</style>"
        app
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

main :: IO ()
main = runApp $ do
    jsValLangMemorizerName <- jsg "localStorage" # "getItem" $ [ "langMemorizerName" ]
    jsValLangMemorizerNameIsNull <- valIsNull jsValLangMemorizerName
    langMemorizerName <- valToStr jsValLangMemorizerName
    dislikedSetsIds <- M.getLocalStorage
        $ ms "dislikedSetsIds" :: M.JSM (Either String [SetIx])
    likedSetsIds <- M.getLocalStorage $ ms "likedSetsIds" :: M.JSM (Either String [SetIx])
    memorizing <- M.getLocalStorage
        $ ms "memorizing" :: M.JSM (Either String MM.Memorizing)
    sets' <- M.getLocalStorage $ ms "sets"
        :: M.JSM
            (Either
                String
                [Either
                    Set
                    (Either SharedSet (Either MM.BeingDownloadedSet MM.DownloadedSet))])
    settings' <- M.getLocalStorage $ ms "settings" :: M.JSM (Either String MM.Settings)
    statistics' <- M.getLocalStorage
        $ ms "statistics" :: M.JSM (Either String [[MM.SetResult]])
    uri <- M.getCurrentURI
    let
        sets       = sets' ^? CLC._Right ^. CLC.non []
        settings   = settings' ^? CLC._Right ^. CLC.non defaultSettings
        statistics = statistics' ^? CLC._Right ^. CLC.non []
    M.startApp M.App
        { events        = M.defaultEvents
        , initialAction =
            if is CLC._Right memorizing
            then MA.DoNothing
            else MA.RepeatMemorizing
        , logLevel      = M.Off
        , model         = MM.Model
            { _activeSetIx       = -1
            , _activeSetsType    = MM.Local
            , _dislikedSetsIds   = dislikedSetsIds ^? _Right ^. non []
            , _editedSet         = MM.EditedSet (ms "") []
            , _langMemorizerName =
                if jsValLangMemorizerNameIsNull then Nothing else Just langMemorizerName
            , _likedSetsIds      = likedSetsIds ^? _Right ^. non []
            , _liteSharedSets    = []
            , _memorizing        = memorizing ^? CLC._Right ^. CLC.non (MM.Memorizing
                { _answer          = ms ""
                , _liteSets        = []
                , _pause           = False
                , _progress        = []
                , _initLiteSetsLen = -1
                , _setIx           = -1
                , _translateIx     = -1
                , _unitIx          = -1
                })
            , _menuIsVisible     = False
            , _pagination        = MM.Pagination
                { _sets       = MM.SetsPagination
                    { _downloaded  = MM.Pages 0 . pagesCount
                        (settings ^. setsPageCount.CLC.to read)
                        $ sets
                            ^.. CLC.traversed.CLC._Right.CLC._Right.CLC._Right
                            ^. CLC.to length
                    , _myLocal     = MM.Pages 0 . pagesCount
                        (settings ^. setsPageCount.CLC.to read)
                        $ sets ^.. CLC.traversed.CLC._Left ^. CLC.to length
                    , _myShared    = MM.Pages 0 . pagesCount
                        (settings ^. setsPageCount.CLC.to read)
                        $ sets ^.. CLC.traversed.CLC._Right._Left ^. CLC.to length
                    , _theirShared = MM.Pages 0 0
                    }
                , _statistics = MM.Pages 0 . pagesCount
                    (settings ^. statisticsPageCount.CLC.to read)
                    $ statistics ^. CLC.to length
                , _units      = sets
                    ^.. CLC.each.to set'.units.CLC.non [].CLC.to
                        ( MM.Pages 0
                        . (pagesCount $ settings ^. unitsPageCount.CLC.to read)
                        . length
                        )
                }
            , _sets              = sets
            , _settings          = settings
            , _statistics        = statistics
            , _uri               = uri
            }
        , mountPoint    = Nothing
        , subs          = [ M.uriSub MA.HandleUri ]
        , update        = updateModel
        , view          = root
        }
  where
    defaultSettings = MM.Settings
        { _activeSetIxs        = Nothing
        , _darkMode            = Just "False"
        , _memorizingMode      = MM.Text
        , _setsPageCount       = "42"
        , _statisticsPageCount = "1"
        , _unitsPageCount      = "42"
        }
