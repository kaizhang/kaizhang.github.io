{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Compiler.Publication where

import           Hakyll
import Hakyll.Web.Pandoc
import Data.Default (def)
import Text.Pandoc.Definition
import Citeproc.Types (Reference(..), Val(..), lookupVariable, Name(..), Date(..), DateParts(..))
import Text.Pandoc.Builder (Inlines, toList)
import Text.Printf (printf)
import qualified Data.Text as T
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Class (runPure)
import Data.List (intersperse)
import Data.Maybe (fromJust)

import Lib.BibTeX

pubCompiler :: Compiler (Item String)
pubCompiler = fmap (toHtml . map formatBib. parseBib . T.pack) <$> getResourceString
  where
    toHtml x = case runPure (writeHtml5String def $ Pandoc mempty x) of
        Left err -> error $ show err
        Right r -> T.unpack r <> js
    js = "<script async src='https://badge.dimensions.ai/badge.js'></script>" <>
        "<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>" <>
        "<script type='text/javascript' src='//cdn.plu.mx/widget-popup.js'></script>"

{-
  pandocCompilerWithTransform defaultHakyllReaderOptions
    defaultHakyllWriterOptions f
  where
    f (Pandoc meta blks) = Pandoc meta $ concatMap f blks
      where
        f (Div _ bs) = map mkSection $ splitByHeader bs
        f x = [x]
        -}

parseBib :: T.Text -> [Reference Inlines]
parseBib txt = case readBibtexString Bibtex mempty (const True) txt of
    Left err -> error $ show err
    Right x -> x

formatBib :: Reference Inlines -> Block
formatBib ref = Div ("", ["bibliography"], []) [LineBlock
    [ title, authors
    , [Strong journal, Str ", ", date, Str ". "]
    , doi
    ] ]
  where
    title = case lookupVariable "title" ref of
        Just (FancyVal x) -> toList x
        _ -> error ""
    authors = case lookupVariable "author" ref of
        Just (NamesVal names) -> intersperse (Str ", ") $ map fromName names
        _ -> error ""
    journal = case lookupVariable "container-title" ref of
        Just (FancyVal x) -> toList x
        _ -> error ""
    doi = case lookupVariable "doi" ref of
        Just (TextVal x) -> map (RawInline "html" . T.pack . (\s -> printf s $ T.unpack x))
            [ "<div data-badge-popover='right' data-doi='%s' data-condensed='true' data-hide-no-mentions='true' class='altmetric-embed'></div>"
            , "<span class='__dimensions_badge_embed__' data-doi='%s' data-style='large_rectangle'></span>"
            , "<a href='https://plu.mx/plum/a/?doi=%s' data-popup='right' data-badge='true' class='plumx-plum-print-popup' data-site='plum' data-hide-when-empty='true'></a>"
            ]
        _ -> error ""
    date = case lookupVariable "issued" ref of
        Just (DateVal (Date [DateParts x] _ _ _)) -> Str $ T.pack $ show $ head x
        _ -> error ""
    fromName n@Name{..} = Str $ fromJust nameGiven <> " " <> fromJust nameFamily