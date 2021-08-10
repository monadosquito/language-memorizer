module Main
    ( main
    ) where

import Adapters.PostgreSQL (PostgreSQLConn ())
import Adapters.Servant (Servant ())
import Ports.Database (DbConnection (connect))
import Ports.WwwServer (run)


main :: IO ()
main = (run =<< (connect :: IO PostgreSQLConn) :: IO Servant) >> pure ()
