{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
import           Hakyll
import Hakyll.Web.Sass (sassCompilerWith, sassDefConfig, SassOptions(..))
import Data.Yaml (decodeFileEither)

import Lib.Compiler.Publication (pubCompiler)

saasOptions = sassDefConfig
    { sassIncludePaths      = Just [ "third_party/foundation-sites/scss"
                                   , "third_party/motion-ui/src" ]
    }

--------------------------------------------------------------------------------
main :: IO ()
main = do
    members <- decodeFileEither "web/pages/people.yml" >>= \case
        Left err -> error $ show err
        Right x -> return x

    hakyll $ do
        match "web/templates/*" $ compile templateBodyCompiler

        match "web/static/img/*" $ do
            route $ gsubRoute "web/" (const "")
            compile copyFileCompiler

        match "web/static/css/*.css" $ do
            route $ gsubRoute "web/" (const "")
            compile copyFileCompiler

        match "web/static/css/app.scss" $ do
            route $ composeRoutes (gsubRoute "web/" (const "")) $ setExtension "css"
            compile $ sassCompilerWith saasOptions

        match "web/static/js/*.js" $ do
            route $ gsubRoute "web/" (const "")
            compile copyFileCompiler

        match "third_party/foundation-sites/dist/js/foundation.min.js" $ do
            route $ constRoute "static/js/foundation.min.js"
            compile copyFileCompiler

        match "web/pages/*.html" $ do
            route $ gsubRoute "web/pages/" $ const ""
            compile $ getResourceBody >>= loadAndApplyTemplate
                "web/templates/default.html" defaultContext >>=
                relativizeUrls

        match "web/pages/publications.bib" $ do
            route $ constRoute "publications.html"
            compile $ pubCompiler members >>= loadAndApplyTemplate
                "web/templates/default.html" (constField "menu2-active" "true" <> defaultContext) >>=
                relativizeUrls