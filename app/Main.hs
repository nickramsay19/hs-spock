{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.IORef
--import Data.Monoid
import qualified Data.Text as T

data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

main :: IO ()
main =
    do ref <- newIORef 1
       spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
       runSpock 8080 (spock spockCfg app)



app :: SpockM () MySession MyAppState ()
app = do 
    get root $
        text "Hello World!"
    get ("about") $
        text "about page"
    get ("sqrt" <//> var) $ \num -> do
        text (T.pack (show (sqrt (read num :: Double))))
    get ("hello" <//> var) $ \name -> do 
        (DummyAppState ref) <- getState
        visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
        text ("Hello " <> name <> ", you are visitor number " <> T.pack (show visitorNumber))