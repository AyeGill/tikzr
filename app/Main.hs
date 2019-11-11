module Main where

import Lib (home, dumbServe, editFileView, editFileSubmit, serveFlat)
import Happstack.Server

import Control.Monad (msum)

myPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)

main :: IO ()
main = simpleHTTP nullConf $ do
    decodeBody myPolicy
    msum [
          dir "test" $ dumbServe
        , dir "form" $ editFileView
        , dir "edit" $ editFileSubmit
        , dir "view" $ serveFlat "."
        , home]