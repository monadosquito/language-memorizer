module Ports.WwwServer
    ( WwwServer (..)
    ) where


import Ports.Database (DbConnection ())

class WwwServer w where
    run :: DbConnection d => d -> IO w
