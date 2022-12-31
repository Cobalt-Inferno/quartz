{-# LANGUAGE OverloadedStrings #-}

module Fetch where

import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit.Combinators (sinkFile)
import Data.Text (Text, splitOn)
import Network.HTTP.Client.Conduit (parseRequest)
import Network.HTTP.Simple (httpSink)
import Text.StringConvert (toString)

-- We can use fetchFile when git or mercurial is not available.
fetchFile :: Text -> IO ()
fetchFile url =
  parseRequest sUrl >>= \request ->
    runResourceT $
    httpSink request $ \_ -> sinkFile $ toString $ last $ splitOn "/" url
  where
    sUrl = toString url
