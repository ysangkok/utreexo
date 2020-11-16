{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import GoImplFunctions (forestWithLeaves, addToForest, swapNodes, printTree)
import Lib (testLeaves, Pos, toCBTree, tree21, transTree, testLeaves2, updateDirt, Height(..), Pos(..), cbTreeToDataTree, sortLeaves, CBTree(..))
import UnitTests (unitTests)
import PropertyTests (propertyTests)
import Lib ( CBTree)
import Forest (CLeaf(CLeaf))
--import Forest (CLeaf(CLeaf))

import System.Exit (die)

import qualified Text.PrettyPrint as PP
import Text.PrettyPrint (render)
import Data.TreeDiff.Tree (treeDiff, EditTree(EditNode), Edit(Swp, Ins, Cpy, Del))
import qualified Data.Tree
--import Data.WideWord.Word128 (byteSwapWord128)
import Control.Lens.Operators ((%~), (^?))
import Control.Lens.At (ix)
import Control.Monad (forM_, unless)
import Control.Lens.Indexed (FunctorWithIndex(imap), FoldableWithIndex(ifoldMap))

import Test.Tasty.Hspec (testSpec)
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (Spec, shouldBe, runIO, it)

import Data.Function((&))
import Data.Word (Word64)
import Data.Aeson (eitherDecodeFileStrict', FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Casing

import GHC.Generics (Generic)

data Obj = Obj {
      objFrom :: Word64
    , objTo :: Word64
} deriving (Show, Generic)

instance ToJSON Obj where
   toJSON = genericToJSON $ aesonPrefix pascalCase
instance FromJSON Obj where
   parseJSON = genericParseJSON $ aesonPrefix pascalCase


testsHspec :: Spec
testsHspec = do
    eitherTestCases <- runIO $ eitherDecodeFileStrict' "updateDirt.json"
    let Right (jsonRaw :: [([Word64], [Obj], Int, Int, [Word64])]) = eitherTestCases
    forM_ (take 200 jsonRaw) $ \(a, b, c, d, e) ->
      let
        tupls = [(objFrom o, objTo o) | o <- b]
      in
        it (show (a, tupls, c, d) ++ " matches reference " ++ show e) $ updateDirt a tupls c d `shouldBe` e

main :: IO ()
main = do
    --let mid = CLeaf (byteSwapWord128 0x3048a770d49f19ee8b5989862037a8fa) (byteSwapWord128 0xd3d7ec71b67ae11ca80aac6a9a2c3adb)
    --_ <- mapM (p . rewriteOf uniplate (\x -> case x of CBNode _ _ n _ _ | n == mid -> Just CBEmpty; _ -> Nothing)) f21

    let Just forest = forestWithLeaves testLeaves
    --let Just forest2 = prepareInsertion forest 12
    let Just forest3 = addToForest forest testLeaves2
    --let Just forest4 = deleteFromForest forest3 [0, 2, 4]
    putStrLn $ printTree forest3
    let Just forest6 = swapNodes forest3 8 9 1 -- swapping these two nodes on level 1 (just above leaves) will also swap the leaves
    putStrLn $ printTree forest6
    --let Just forest5 = addToForest forest4 testLeaves3

    --print $ f2 ^? (ix 1).chldr._1 == toDiagramsTree forest6 ^? (ix 1).chldr._1

    let Just forest21T0 = forestWithLeaves tree21
    putStrLn "printTree (convert to Data.Tree and drawTree)"
    _ <- forM_ (toCBTree forest21T0) $ \t -> do
      let (x :: [Pos]) = ifoldMap (\i _ -> return i) t
      print x
      let im = imap (\i a -> show (i, a)) t
      putStrLn $ Data.Tree.drawTree $ fmap (\(_, _, cleaf) -> cleaf) $ cbTreeToDataTree im

    let f2 :: [CBTree CLeaf] = (toCBTree forest3) & ix 1 %~ transTree
    let Just t1 = f2 ^? (ix 1)
    let dt = cbTreeToDataTree t1
    putStrLn "Original Go forest"
    putStrLn $ Data.Tree.drawForest (map (fmap show) $ map cbTreeToDataTree (toCBTree forest3))
    putStrLn "Go forest"
    putStrLn $ Data.Tree.drawForest (map (fmap show) $ map cbTreeToDataTree (toCBTree forest6))
    putStrLn "HS forest (original)"
    let Just t0 = (toCBTree forest3) ^? (ix 1)
    let dt0 = cbTreeToDataTree t0
    putStrLn $ Data.Tree.drawTree (fmap show dt0)
    putStrLn "HS forest (swapped)"
    putStrLn $ Data.Tree.drawTree (fmap show dt)

    unless (f2 == (toCBTree forest6)) $
      let
        goRoot = Data.Tree.Node (Height 0, Pos 0, CLeaf 0 0) (map cbTreeToDataTree (toCBTree forest6))
        hsRoot = Data.Tree.Node (Height 0, Pos 0, CLeaf 0 0) (map (cbTreeToDataTree) f2)
      in do
        putStrLn "Difference:"
        putStrLn $ render $ ppEditTree (PP.text . show) (treeDiff goRoot hsRoot)

        -- TODO make an actual unit test
        let unsorted = (CBNode (Height 2) (Pos 5) () (CBNode (Height 2) (Pos 3) () CBEmpty CBEmpty) (CBNode (Height 2) (Pos 2) () CBEmpty CBEmpty))
        putStrLn "This (below) should show that the positions are swapped. But we are just swapping values..."
        putStrLn $ show $ cbTreeToDataTree $ sortLeaves unsorted
        --print $ unsorted ^. partsOf (itraversed . _2 . filtered isLeaf)
        die "Trees differed"

    putStrLn "running test suite..."
    _ <- propertyTests

    tree <- testSpec "hspec tests" testsHspec
    defaultMain (unitTests tree)

-- Copied from tree-diff source
ppTree :: (a -> PP.Doc) -> Data.Tree.Tree a -> PP.Doc
ppTree pp = ppT
  where
    ppT (Data.Tree.Node x []) = pp x
    ppT (Data.Tree.Node x xs) = PP.parens $ PP.hang (pp x) 2 $
        PP.sep $ map ppT xs

-- Copied from tree-diff source
ppEditTree :: (a -> PP.Doc) -> Edit (EditTree a) -> PP.Doc
ppEditTree pp = PP.sep . ppEdit
  where
    ppEdit (Cpy tree) = [ ppTree tree ]
    ppEdit (Ins tree) = [ PP.char '+' PP.<> ppTree tree ]
    ppEdit (Del tree) = [ PP.char '-' PP.<> ppTree tree ]
    ppEdit (Swp a b) =
        [ PP.char '-' PP.<> ppTree a
        , PP.char '+' PP.<> ppTree b
        ]

    ppTree (EditNode x []) = pp x
    ppTree (EditNode x xs) = PP.parens $ PP.hang (pp x) 2 $
       PP.sep $ concatMap ppEdit xs
