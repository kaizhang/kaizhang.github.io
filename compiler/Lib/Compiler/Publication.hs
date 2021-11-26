{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Compiler.Publication where

import           Hakyll
import Hakyll.Web.Pandoc
import Data.Default (def)
import Text.Pandoc.Definition
import Citeproc.Types (Reference(..), Val(..), lookupVariable, Name(..), Date(..), DateParts(..), toText)
import Text.Pandoc.Builder (Inlines, toList)
import Text.Printf (printf)
import qualified Data.Text as T
import Text.Pandoc.Writers.HTML (writeHtml5)
import Text.Pandoc.Class (runPure)
import Data.List (intersperse, sortBy, groupBy)
import Data.Ord
import Data.Function (on)
import Data.Maybe (fromJust)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import qualified Text.Blaze.Html.Renderer.String as H

import Lib.BibTeX

pubCompiler :: Compiler (Item String)
pubCompiler = fmap (f . groupBib . map formatBib . parseBib . T.pack) <$> getResourceString
  where
    f x = H.renderHtml x <> js
    js = "<script async src='https://badge.dimensions.ai/badge.js'></script>" <>
        "<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>" <>
        --"<script type='text/javascript' src='//cdn.plu.mx/widget-details.js'></script>"
        "<script type='text/javascript' src='//cdn.plu.mx/widget-popup.js'></script>"


parseBib :: T.Text -> [Reference Inlines]
parseBib txt = case readBibtexString Bibtex mempty (const True) txt of
    Left err -> error $ show err
    Right x -> x

data MetaData = MetaData
    { _year :: Int
    , _journal :: T.Text
    }

groupBib :: [(MetaData, Block)] -> H.Html
groupBib refs = H.div H.! H.class_ "grid-x" $ do
    H.div H.! H.class_ "cell large-1 medium-2 show-for-medium" $
        H.nav H.! H.class_ "sticky-container" H.! H.dataAttribute "sticky-container" mempty $
        H.div H.! H.class_ "sticky" H.! H.dataAttribute "sticky" mempty H.! H.dataAttribute "anchor" "main" $
        H.ul H.! H.class_ "vertical menu" H.! H.dataAttribute "magellan" mempty $
            mapM_ mkMenu refGroup
    H.div H.! H.class_ "sections cell auto text-justify" $ mapM_ mkRef refGroup
  where
    refGroup = map (\x -> (_year $ fst $ head x, map snd x)) $
        groupBy ((==) `on` (_year . fst)) $ sortBy (flip (comparing (_year . fst))) refs
    mkRef (year, blks) = H.section H.! H.id (H.toValue year) H.!
        H.dataAttribute "magellan-target" (H.toValue year) $ toHtml blks
    mkMenu (year, _) = H.li $ H.a H.! H.href (H.toValue $ "#" <> show year) $ H.toHtml year
      
toHtml :: [Block] -> H.Html
toHtml x = case runPure (writeHtml5 def $ Pandoc mempty x) of
    Left err -> error $ show err
    Right r -> r

formatBib :: Reference Inlines -> (MetaData, Block)
formatBib ref = (MetaData year $ toText journal, content)
  where
    content = Div ("", ["reference-item"], [])
        [ LineBlock [[title], authors]
        , Div ("", ["grid-x", "align-middle"], [])
            [ Div ("", ["cell", "shrink"], []) [plumx]
            , Div ("", ["cell", "shrink"], []) [Plain [Strong $ toList journal, Str ", ", Str $ T.pack $ show year, Str ". "]]
            , Div ("", ["cell", "shrink"], []) [altmetric]
            ]
        ]
    title = case lookupVariable "title" ref of
        Just (FancyVal x) -> Link nullAttr (toList x) ("https://doi.org/" <> doi, "")
        _ -> error ""
    authors = case lookupVariable "author" ref of
        Just (NamesVal names) -> intersperse (Str ", ") $ map fromName names
        _ -> error ""
    journal = case lookupVariable "container-title" ref of
        Just (FancyVal x) -> x
        _ -> error ""
    doi = case lookupVariable "doi" ref of
        Just (TextVal x) -> x
        _ -> error ""
    altmetric = RawBlock "html" $ T.pack $ printf
        "<div data-badge-popover='right' data-doi='%s' data-condensed='true' data-hide-no-mentions='true' class='altmetric-embed'></div>"
        $ T.unpack doi
        --, "<span class='__dimensions_badge_embed__' data-doi='%s' data-style='large_rectangle'></span>"
    plumx = RawBlock "html" $ T.pack $ printf
        "<a href='https://plu.mx/plum/a/?doi=%s' data-popup='left' data-size='small' class='plumx-plum-print-popup' data-site='plum' data-hide-when-empty='true'></a>"
        $ T.unpack doi
    year = case lookupVariable "issued" ref of
        Just (DateVal (Date [DateParts x] _ _ _)) -> head x
        _ -> error ""
    fromName n@Name{..} = Str $ fromJust nameGiven <> " " <> fromJust nameFamily