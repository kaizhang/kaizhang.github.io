{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Lib.Compiler.Publication
    ( pubCompiler
    , isSelected
    , readBib
    ) where

import           Hakyll
import Hakyll.Web.Pandoc
import Data.Default (def)
import Text.Pandoc.Definition
import Citeproc.Types (Reference(..), Val(..), lookupVariable, Name(..), Date(..), DateParts(..), toText)
import Text.Pandoc.Builder (Inlines, toList)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc.Writers.HTML (writeHtml5)
import Text.Pandoc.Class (runPure)
import Data.List (intersperse, sortBy, groupBy)
import Data.Ord
import Data.Function (on)
import Data.Maybe (fromJust, fromMaybe)
import Data.Char (isLetter, isSeparator)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import qualified Text.Blaze.Html.Renderer.String as H
import           Data.Time                       (toGregorian)

import Lib.BibTeX
import Lib.Types

{-
pubCompiler :: (Reference Inlines -> Bool) -> [Member] -> Compiler (Item String)
pubCompiler filterFn members =
    fmap (f . groupBib . map (formatBib members) . filter filterFn . parseBib . T.pack) <$> getResourceString
  where
    f x = H.renderHtml x <> js
    js = "<script async src='https://badge.dimensions.ai/badge.js'></script>" <>
        "<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>" <>
        --"<script type='text/javascript' src='//cdn.plu.mx/widget-details.js'></script>"
        "<script type='text/javascript' src='//cdn.plu.mx/widget-popup.js'></script>"
        -}

pubCompiler :: [Member] -> [Reference Inlines] -> Compiler (Item String)
pubCompiler members refs = fmap (fmap render) $ getResourceString >>= readPandoc
  where
    render (Pandoc _ blk) = H.renderHtml (groupBib blk $ map (formatBib members) refs) <> js
    js = "<script async src='https://badge.dimensions.ai/badge.js'></script>" <>
        "<script type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>" <>
        --"<script type='text/javascript' src='//cdn.plu.mx/widget-details.js'></script>"
        "<script type='text/javascript' src='//cdn.plu.mx/widget-popup.js'></script>"

readBib :: FilePath -> IO [Reference Inlines]
readBib fl = (readBibtexString Bibtex mempty (const True) <$> T.readFile fl) >>= \case
    Left err -> error $ show err
    Right x -> return x

data MetaData = MetaData
    { _year :: Int
    , _journal :: T.Text
    }

groupBib :: [Block] -> [(MetaData, Block)] -> H.Html
groupBib txt refs = H.div H.! H.class_ "grid-x" $ do
    H.div H.! H.class_ "cell large-1 medium-2 show-for-medium" $
        H.nav H.! H.class_ "sticky-container" H.! H.dataAttribute "sticky-container" mempty $
        H.div H.! H.class_ "sticky" H.! H.dataAttribute "sticky" mempty H.!
        H.dataAttribute "anchor" "main" H.! H.dataAttribute "margin-top" "5" $
        H.ul H.! H.class_ "vertical menu" H.! H.dataAttribute "magellan" mempty $
            mapM_ mkMenu refGroup
    H.div H.! H.class_ "sections cell auto medium-text-justify" $ do
        blkToHtml txt
        mapM_ mkRef refGroup
  where
    refGroup = map (\x -> (_year $ fst $ head x, map snd x)) $
        groupBy ((==) `on` (_year . fst)) $ sortBy (flip (comparing (_year . fst))) refs
    mkRef (year, blks) = H.section H.! H.id (H.toValue year) H.!
        H.dataAttribute "magellan-target" (H.toValue year) $ blkToHtml blks
    mkMenu (year, _) = H.li $ H.a H.! H.href (H.toValue $ "#" <> show year) $ H.toHtml year
      
blkToHtml :: [Block] -> H.Html
blkToHtml x = case runPure (writeHtml5 def $ Pandoc mempty x) of
    Left err -> error $ show err
    Right r -> r

formatBib :: [Member] -> Reference Inlines -> (MetaData, Block)
formatBib members ref = (MetaData year $ toText journal, content)
  where
    content = Div ("", ["reference-item"], [])
        [ LineBlock [[title], authors]
        , Div ("", ["grid-x", "align-middle"], [])
            [ Div ("", ["cell", "shrink"], []) [plumx]
            , Div ("", ["cell", "shrink"], []) [Plain [Strong $ toList journal, Str $ " (" <> T.pack (show year) <> ") "]]
            , Div ("", ["cell", "shrink"], []) [altmetric]
            , Div ("", ["cell", "shrink"], []) [dimensions]
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
    year = case lookupVariable "issued" ref of
        Just (DateVal (Date [DateParts x] _ _ _)) -> head x
        _ -> error ""
    fromName n@Name{..} | isPresent year n members = Strong [Str $ fromJust nameGiven <> " " <> fromJust nameFamily]
                        | otherwise = Str $ fromJust nameGiven <> " " <> fromJust nameFamily

    altmetric = RawBlock "html" $ T.pack $ printf
        "<div data-badge-popover='right' data-doi='%s' data-condensed='true' data-hide-no-mentions='true' class='altmetric-embed'></div>"
        $ T.unpack doi
    dimensions = RawBlock "html" $ T.pack $ printf
        "<span class='__dimensions_badge_embed__' data-doi='%s' data-style='small_rectangle'></span>"
        $ T.unpack doi
    plumx = RawBlock "html" $ T.pack $ printf
        "<a href='https://plu.mx/plum/a/?doi=%s' data-popup='right' data-size='small' class='plumx-plum-print-popup' data-site='plum' data-hide-when-empty='true'></a>"
        $ T.unpack doi

isPresent :: Int -> Name -> [Member] -> Bool
isPresent year Name{..} members = any f members
  where
    f x = let present = fromMaybe True $ (\(x,_,_) -> fromIntegral x >= year) . toGregorian <$> endYear x
          in nm == name x && present
    nm = T.filter (\x -> isLetter x || isSeparator x) $ fromJust nameGiven <> " " <> fromJust nameFamily
    
isSelected :: Reference Inlines -> Bool
isSelected ref = elem "Kai Zhang" $ map (T.filter (\x -> isLetter x || isSeparator x)) $
    filter (\x -> T.last x == '*' || T.last x == '#') authors <> [head authors, last authors]
  where
    authors = case lookupVariable "author" ref of
        Just (NamesVal names) -> map fromName names
        _ -> error ""
    fromName Name{..} = fromJust nameGiven <> " " <> fromJust nameFamily