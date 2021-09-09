{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

module Nontouchscreen
    ( main
    ) where

#ifndef __GHCJS__
import Language.Javascript.JSaddle ((!), (<#))
import Language.Javascript.JSaddle.Warp (run)
import System.Directory (makeAbsolute)
import Text.Sass.Compilation (StringResult, compileFile)
import Text.Sass.Options (defaultSassOptions)
#endif
import Control.Lens ((^.), (^..), (^?))
import Control.Lens.Extras (is)
import Control.Lens.TH (makeFieldsNoPrefix)
import Miso.String (ms)

import System.Environment (getEnv)

import Model.UpdateModel (updateModel)
import Utils (pagesCount)
import Views.Dumb.Providing.Root.Nontouchscreen (root)

import qualified Control.Lens.Combinators as CLC
import qualified Miso as M

import qualified Model.Action as MA
import qualified Model.Model as MM


makeFieldsNoPrefix ''MM.LiteSet
makeFieldsNoPrefix ''MM.Set
makeFieldsNoPrefix ''MM.Settings

#ifndef __GHCJS__
runApp :: M.JSM () -> IO ()
runApp app = do
    sassMainPath <- makeAbsolute "src/Views/Dumb/Providing/Root/Nontouchscreen.sass"
    Right css <- compileFile sassMainPath defaultSassOptions :: StringResult
    port <- getEnv "nontouchscreen_client_dev_www_server_port"
    run (read port) $ do
        M.getDoc ! "head" <# "innerHTML" $ "<style>" ++ css ++ "</style>"
        app
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

main :: IO ()
main = runApp $ do
    memorizing <- M.getLocalStorage
        $ ms "memorizing" :: M.JSM (Either String MM.Memorizing)
    sets' <- M.getLocalStorage $ ms "sets" :: M.JSM (Either String [MM.Set])
    settings' <- M.getLocalStorage $ ms "settings" :: M.JSM (Either String MM.Settings)
    uri <- M.getCurrentURI
    let
        sets     = sets' ^? CLC._Right ^. CLC.non []
        settings = settings' ^? CLC._Right ^. CLC.non defaultSettings
    M.startApp M.App
        { events        = M.defaultEvents
        , initialAction =
            if is CLC._Right memorizing
            then MA.DoNothing
            else MA.RepeatMemorizing
        , logLevel      = M.Off
        , model         = MM.Model
            { _activeSetIx = -1
            , _editedSet   = MM.EditedSet (ms "") [] 
            , _memorizing  = memorizing ^? CLC._Right ^. CLC.non (MM.Memorizing
                { _answer          = ms ""
                , _liteSets        = []
                , _pause           = False
                , _progress        = []
                , _initLiteSetsLen = -1
                , _setIx           = -1
                , _translateIx     = -1
                , _unitIx          = -1
                })
            , _menuIsVisible = False
            , _pagination    = MM.Pagination
                { _sets  = MM.Pages 0 . pagesCount
                    (settings ^. setsPageCount.CLC.to read)
                    $ sets ^. CLC.to length
                , _units = sets
                    ^.. CLC.each.units.CLC.non [].CLC.to
                        ( MM.Pages 0
                        . (pagesCount $ settings ^. unitsPageCount.CLC.to read)
                        . length
                        )
                }
            , _sets        = sets
            , _settings    = settings
            , _uri         = uri
            }
        , mountPoint    = Nothing
        , subs          = [ M.uriSub MA.HandleUri ]
        , update        = updateModel
        , view          = root
        }
  where
    defaultSettings = MM.Settings
        { _activeSetIxs   = Nothing
        , _memorizingMode = MM.Text
        , _setsPageCount  = "42"
        , _unitsPageCount = "42"
        }
