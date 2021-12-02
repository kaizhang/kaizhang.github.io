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
import Citeproc.Types (Reference(..), Val(..), lookupVariable, Name(..), Date(..), DateParts(..), toText, unItemId)
import Text.Pandoc.Builder (Inlines, toList)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Pandoc.Writers.HTML (writeHtml5)
import Text.Pandoc.Class (runPure)
import Data.List (intercalate, sortBy, groupBy)
import Data.Ord
import Data.Function (on)
import Data.Maybe (fromJust, fromMaybe, maybe)
import Data.Char (isLetter, isSeparator)
import qualified Text.Blaze.Html5 as H hiding (style)
import qualified Text.Blaze.Html5.Attributes as H
import qualified Text.Blaze.Html.Renderer.String as H
import           Data.Time                       (toGregorian)

import Lib.BibTeX
import Lib.Types

pubCompiler :: [Member] -> [Reference Inlines] -> Compiler (Item String)
pubCompiler members refs = fmap (fmap render) $ getResourceString >>= readPandoc
  where
    render (Pandoc _ blk) = H.renderHtml (groupBib blk $ map (formatBib members) refs) <> js
    js = "<script async src='https://badge.dimensions.ai/badge.js'></script>" <>
        --"<script async type='text/javascript' src='https://d1bxh8uas1mnw7.cloudfront.net/assets/embed.js'></script>" <>
        "<script async type='text/javascript' src='//cdn.plu.mx/widget-popup.js'></script>"

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
    H.div H.! H.class_ "cell large-1 medium-2 show-for-medium" $ H.nav H.! H.style "position:sticky;top:90px;" $
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
        , Div ("", ["grid-x", "grid-margin-x", "align-middle"], []) $
            [ Div ("", ["cell", "shrink"], []) [plumx]
            , Div ("", ["cell", "shrink"], []) [Plain [Strong $ toList journal, Str $ " (" <> T.pack (show year) <> ") "]]
            --, Div ("", ["cell", "shrink"], []) [altmetric]
            , Div ("", ["cell", "shrink"], []) [dimensions]
            , Div ("", ["cell", "show-for-medium", "medium-1"], []) []
            , Div ("", ["cell", "shrink"], []) [Plain [Str "Media:"]]
            , Div ("", ["cell", "shrink"], []) [pdf]
            ] <> maybe [] (return . Div ("", ["cell", "shrink"], [])) software
              <> maybe [] (return . Div ("", ["cell", "shrink"], [])) website
              <> maybe [] (return . Div ("", ["cell", "shrink"], [])) video
        ]
    rid = unItemId $ referenceId ref
    title = let FancyVal x = lookupVariable' "title" ref
            in Link nullAttr (toList x) ("https://doi.org/" <> doi, "")
    authors = let NamesVal names = lookupVariable' "author" ref
              in intercalate [Str ", "] $ map fromName names
    journal = let FancyVal x = lookupVariable' "container-title" ref in x
    doi = let TextVal x = lookupVariable' "doi" ref in x
    year = let DateVal (Date [DateParts x] _ _ _) = lookupVariable' "issued" ref in head x
    fromName n@Name{..} =
        let lastName = if T.last (fromJust nameFamily) == '#'
                then [Str $ T.init $ fromJust nameFamily, Superscript [Str "#"]]
                else [Str $ fromJust nameFamily]
            fullName = [Str $ fromJust nameGiven, Space] <> lastName
        in if isPresent year n members then [Strong fullName] else fullName

    altmetric = RawBlock "html" $ T.pack $ printf
        "<div data-badge-popover='top' data-doi='%s' data-condensed='true' data-hide-no-mentions='true' class='altmetric-embed'></div>"
        $ T.unpack doi
    dimensions = RawBlock "html" $ T.pack $ printf
        "<span class='__dimensions_badge_embed__' data-doi='%s' data-style='small_rectangle' data-legend='hover-top'></span>"
        $ T.unpack doi
    plumx = RawBlock "html" $ T.pack $ printf
        "<a href='https://plu.mx/plum/a/?doi=%s' data-popup='top' data-size='small' class='plumx-plum-print-popup' data-site='plum' data-hide-when-empty='true'></a>"
        $ T.unpack doi
    pdf = mkIcon "far fa-file-pdf" "red"  "PDF" $ pubRootUrl <> rid <> ".pdf" 
    software = (\(TextVal x) -> [mkIcon "far fa-file-code" "black" "Software" x]) <$> lookupVariable "software" ref
    video = (\(TextVal x) -> [mkIcon "fas fa-film" "cyan" "Video" x]) <$> lookupVariable "video" ref
    website = (\(TextVal x) -> [mkIcon "fas fa-home" "#1779ba" "Website" x]) <$> lookupVariable "website" ref
    mkIcon icon color tt link = RawBlock "html" $ T.pack $ printf
        "<span data-tooltip data-click-open='false' title='%s'><a href='%s'><i style='color:%s' class='%s'></i></a>" (tt :: String) (T.unpack link) (color :: String) (icon :: String)
    pubRootUrl = "http://renlab.sdsc.edu/kai/my_paper/"

lookupVariable' key var = case lookupVariable key var of
    Just x -> x
    _ -> error $ "Key is not present: " <> show key

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