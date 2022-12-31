module Fetch where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.Combinators (sinkFile)
import Network.HTTP.Client.Conduit (parseRequest)
import Network.HTTP.Simple (httpSink)
