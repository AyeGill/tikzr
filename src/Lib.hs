{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module Lib
    ( someFunc
    , editFileView
    , editFileSubmit
    , dumbServe
    , serveFlat
    ) where

import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO (readFile, writeFile)
import Data.Char (isDigit, isLetter)
import Prelude hiding (readFile, writeFile)
import Control.Monad.IO.Class (liftIO)
import Happstack.Server hiding (timeout)
import Data.Foldable (asum)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, textarea)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String (fromString, IsString, String)
import System.Timeout
import System.Process (readProcessWithExitCode, callProcess)
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.Directory (doesFileExist)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

serveFlat :: FilePath -> ServerPartT IO Response
serveFlat = serveDirectory DisableBrowsing []

editFileView :: ServerPartT IO Response
editFileView = do ident <- sanitize <$> lookRaw "ident"
                  let path = makePath Tex ident
                  exists <- liftIO $ setup ident
                  if exists
                    then ok $ template "form" $ toHtml ("File existed already!" :: Text)
                    else do
                        body <- liftIO $ readFile $ fromPath path
                        ok $ template "form" $
                                    form ! action (fromString ("/edit/?ident=" ++ fromSane ident)) ! enctype "multipart/form-data" ! A.method "POST" $ do
                                    textarea ! type_ "text" ! A.id "body" ! name "body"  $ toHtml body
                                    input ! type_ "submit" ! value "Submit"

editFileSubmit :: ServerPartT IO Response
editFileSubmit = do method POST
                    ident <- sanitize <$> lookRaw "ident"
                    let path = makePath Tex ident
                    body <- lookText "body"
                    liftIO $ writeFile (fromPath path) body
                    result <- texToSvg ident
                    case result of
                        Compiled -> seeOther ("/view/" ++ fromPath (makePath Svg ident)) $ toResponse ()
                        Failed str -> ok $ template "failed" $
                                                a ! A.href (fromString $ "/form?ident=" ++ fromSane ident) $ 
                                                        "Compilation failed - click here to edit your file"

setup :: SaneString -> IO Bool -- return: did file exist already?
setup ident = do
    figExists <- doesFileExist $ fromPath $ makePath Svg ident
    texExists <- doesFileExist $ fromPath $ makePath Tex ident
    if figExists
        then return True
        else if texExists
            then return False
            else appendFile (fromPath $ makePath Tex ident) stdBody >> return False

stdBody = "\\documentclass{article}\n\
        \\\usepackage{tikz}\n\
        \\\usepackage{tikz-cd}\n\
        \\\begin{document}\n\
        \\n\
        \\\end{document}"


dumbServe :: ServerPartT IO Response
dumbServe = ok $ toResponse $ toHtml ("FoobarBaz" :: Text)

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

texToSvg :: SaneString -> ServerPartT IO Status
texToSvg ident = liftIO $ do
    res1 <- texToDvi ident
    case res1 of
        Compiled -> dviToSvg ident
        Failed str -> return $ Failed str

texToDvi :: SaneString -> IO Status
texToDvi ident = do
    let texPath = makePath Tex ident
    let command = "lualatex" 
    let args = ["--no-shell-escape"
            , "--halt-on-error" 
            , "--nosocket"
            , "--output-format=dvi"
            , fromPath texPath]
    result <- timeout 1000000 $ readProcessWithExitCode command args ""
    case result of
        Nothing -> return $ Failed "latex process timed out!"
        Just (ExitSuccess, _, _) -> return Compiled
        Just (ExitFailure n, _, stderr) -> return $ Failed 
            $ "latex failed with code " ++ show n ++ "stderr: " ++ stderr

dviToSvg :: SaneString -> IO Status
dviToSvg ident = do
    let dviPath = makePath Dvi ident
    let command = "dvisvgm"
    let args = ["--no-fonts", fromPath dviPath]
    result <- timeout 1000000 $ readProcessWithExitCode command args ""
    case result of
        Nothing -> return $ Failed "dvisvgm process timed out!"
        Just (ExitSuccess, _, _) -> return Compiled
        Just (ExitFailure n, _, stderr) -> return $ Failed 
            $ "dvisvgm failed with code " ++ show n ++ "stderr: " ++ stderr




newtype RawString = Raw {fromRaw :: String} deriving IsString --input
newtype SaneString = Sane {fromSane :: String} deriving IsString -- only alphanumericals.
newtype SanePath = Path {fromPath :: String} deriving IsString
data Extension = Tex | Svg | Dvi
data Status = Compiled | Failed String deriving Show


sanitize :: RawString -> SaneString
sanitize = Sane . filter (\x -> isDigit x || isLetter x) . fromRaw

makePath :: Extension -> SaneString -> SanePath
makePath Tex = Path . (++".tex") . fromSane
makePath Svg = Path . (++".svg") . fromSane
makePath Dvi = Path . (++".dvi") . fromSane

lookRaw name = Raw . T.unpack <$> lookText name