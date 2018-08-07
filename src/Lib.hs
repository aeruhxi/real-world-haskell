{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( runApp
  )
where

import           RealWorld.Api                  ( runApi )

runApp :: IO ()
runApp = runApi
