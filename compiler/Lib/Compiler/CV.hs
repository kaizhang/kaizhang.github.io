module Lib.Compiler.CV where

import           Hakyll
import Hakyll.Web.Pandoc
import Text.Pandoc.Definition
import Text.Printf (printf)

cvCompiler :: Compiler (Item String)
cvCompiler = pandocCompilerWithTransform defaultHakyllReaderOptions
    defaultHakyllWriterOptions f
  where
    f (Pandoc meta blks) = Pandoc meta $ concatMap f blks
      where
        f (Div _ bs) = map mkSection $ splitByHeader bs
        f x = [x]

mkSection :: (Block, [Block]) -> Block
mkSection (h, blks) = Div ("", ["grid-x", "section"], []) 
    [ Div ("", ["cell", "medium-2", "side-note"], []) [h]
    , Div ("", ["cell", "medium-10"], []) $ concatMap f blks ]
  where
    f (DefinitionList xs) = map (\(a,b) -> mkGrid a b) xs
    f x = [mkGrid [] [[x]]]
    mkGrid term def = Div ("", ["grid-x"], []) 
        [ Div ("", ["cell", "medium-10"], []) $ concat def
        , Div ("", ["cell", "medium-2", "side-note"], []) [Plain term] ]

splitByHeader :: [Block] -> [(Block, [Block])]
splitByHeader (h@(Header _ _ _) : xs) = (h, a) : splitByHeader b
  where
    (a,b) = span notHeader xs
    notHeader (Header _ _ _) = False
    notHeader _ = True
splitByHeader [] = []
splitByHeader x = error $ "Header not found in: " ++ show x
