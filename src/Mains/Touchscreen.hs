{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

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
import Control.Lens ((^.), (^?))
import Control.Lens.Combinators (_Right, non)
import Miso.String (ms)

import System.Environment (getEnv)

import Model.Action (Action (DoNothing, HandleUri))
import Model.Model (Model (..), Set ())
import Model.UpdateModel (updateModel)
import Views.Dumb.Providing.Root.Touchscreen (root)

import qualified Miso as M


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
    sets <- M.getLocalStorage $ ms "sets" :: M.JSM (Either String [Set])
    M.startApp M.App
        { events        = M.defaultEvents
        , initialAction = DoNothing
        , logLevel      = M.Off
        , model         = Model
            { _sets = sets ^? _Right ^. non []
            , _uri  = uri
            }
        , mountPoint    = Nothing
        , subs          = [ M.uriSub HandleUri ]
        , update        = updateModel
        , view          = root
        }
