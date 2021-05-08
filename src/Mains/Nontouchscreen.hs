{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

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

import System.Environment (getEnv)

import Model.Action (Action (DoNothing))
import Model.UpdateModel (updateModel)
import Views.Dumb.Providing.Root.Nontouchscreen (root)

import qualified Miso as M


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
main = runApp $ M.startApp M.App
    { events        = M.defaultEvents
    , initialAction = DoNothing
    , logLevel      = M.Off
    , model         = ()
    , mountPoint    = Nothing
    , subs          = []
    , update        = updateModel
    , view          = root
    }
