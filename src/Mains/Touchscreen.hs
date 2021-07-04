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
import Language.Javascript.JSaddle ((!), (<#))
import Language.Javascript.JSaddle.Warp (run)
import System.Directory (makeAbsolute)
import Text.Sass.Compilation (StringResult, compileFile)
import Text.Sass.Options (defaultSassOptions)
#endif
import Control.Lens ((^.), (^..), (^?))
import Control.Lens.Combinators (_Right, each, non, to)
import Control.Lens.TH (makeFieldsNoPrefix)
import Miso.String (ms)

import System.Environment (getEnv)

import Model.Action (Action (DoNothing, HandleUri))
import Model.UpdateModel (updateModel)
import Utils (pagesCount)
import Views.Dumb.Providing.Root.Touchscreen (root)

import qualified Miso as M

import qualified Model.Model as MM


makeFieldsNoPrefix ''MM.Set
makeFieldsNoPrefix ''MM.Settings

#ifndef __GHCJS__
runApp :: M.JSM () -> IO ()
runApp app = do
    sassMainPath <- makeAbsolute "src/Views/Dumb/Providing/Root/Touchscreen.sass"
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
    uri <- M.getCurrentURI
    sets' <- M.getLocalStorage $ ms "sets" :: M.JSM (Either String [MM.Set])
    settings' <- M.getLocalStorage $ ms "settings" :: M.JSM (Either String MM.Settings)
    let
        sets = sets' ^? _Right ^. non []
        settings = settings' ^? _Right ^. non defaultSettings
    M.startApp M.App
        { events        = M.defaultEvents
        , initialAction = DoNothing
        , logLevel      = M.Off
        , model         = MM.Model
            { _activeSetIx = -1
            , _editedSet   = MM.EditedSet (ms "") []
            , _pagination  = MM.Pagination
                { _sets  = MM.Pages 0 . pagesCount (settings ^. setsPageCount.to read)
                    $ sets ^. to length
                , _units = sets
                    ^.. each.units.non [].to
                        ( MM.Pages 0
                        . (pagesCount $ settings ^. unitsPageCount.to read)
                        . length
                        )
                }
            , _sets        = sets
            , _settings    = settings
            , _uri         = uri
            }
        , mountPoint    = Nothing
        , subs          = [ M.uriSub HandleUri ]
        , update        = updateModel
        , view          = root
        }
  where
    defaultSettings = MM.Settings
        { _unitsPageCount = "42"
        , _setsPageCount  = "42"
        }
