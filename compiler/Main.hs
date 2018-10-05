{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import Hakyll.Web.Sass (sassCompilerWith, sassDefConfig, SassOptions(..))

saasOptions = sassDefConfig
    { sassIncludePaths      = Just ["web/foundation/css"]
    }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "web/templates/*" $ compile templateBodyCompiler

    match "web/static/img/*" $ do
        route $ gsubRoute "web/" (const "")
        compile copyFileCompiler

    match "web/static/css/*" $ do
        route $ gsubRoute "web/" (const "")
        compile copyFileCompiler

    match "web/static/js/*" $ do
        route $ gsubRoute "web/" (const "")
        compile copyFileCompiler

    -- match "web/static/scss/app.scss" $ do
    --    route $ constRoute "static/css/app.css"
    --    compile $ sassCompilerWith saasOptions

    match "web/pages/*.html" $ do
        route $ gsubRoute "web/pages/" $ const ""
        compile $ do
            getResourceBody
                >>= loadAndApplyTemplate "web/templates/default.html" defaultContext
                >>= relativizeUrls

