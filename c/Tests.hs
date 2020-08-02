{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Lib (forestWithLeaves, addToForest, testLeaves, toGADT, Pos, myTreeToDataTree, toCBTree, cbTreeToBTree, swapNodes, printTree, tree21, transTree, testLeaves2, updateDirt)
import UnitTests (unitTests)
import PropertyTests (propertyTests)
import Lib ( CBTree)
import Forest (CLeaf)
--import Forest (CLeaf(CLeaf))

import qualified Data.Tree
--import Data.WideWord.Word128 (byteSwapWord128)
import Control.Lens.Operators ((%~), (^?))
import Control.Lens.At (ix)
--import Control.Lens.Plated (rewriteOf)
import Control.Monad (forM_)
import Control.Lens.Indexed (FunctorWithIndex(imap), FoldableWithIndex(ifoldMap))

import Test.Tasty.Hspec (testSpec)
import Test.Tasty (defaultMain)
import Test.Tasty.Hspec (Spec, shouldBe, runIO, it)

--import Data.Data.Lens (uniplate)
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
    let f = toGADT forest3

    --print $ f2 ^? (ix 1).chldr._1 == toGADT forest6 ^? (ix 1).chldr._1

    let Just forest21T0 = forestWithLeaves tree21
    putStrLn "printTree (convert to Data.Tree and drawTree)"
    _ <- forM_ (toCBTree forest21T0) $ \t -> do
      let (x :: [Pos]) = ifoldMap (\i _ -> return i) t
      print x
      let im = imap (\i a -> show (i, a)) t
      putStrLn $ Data.Tree.drawTree $ myTreeToDataTree $ cbTreeToBTree im

    let f2 :: [CBTree CLeaf] = (toCBTree forest3) & ix 1 %~ transTree
    let Just t1 = f2 ^? (ix 1)
    let dt = myTreeToDataTree $ cbTreeToBTree $ t1
    putStrLn "Original Go forest"
    putStrLn $ Data.Tree.drawForest (map (fmap show) $ map myTreeToDataTree f)
    putStrLn "Go forest"
    putStrLn $ Data.Tree.drawForest (map (fmap show) $ map myTreeToDataTree (toGADT forest6))
    putStrLn "HS forest (original)"
    let Just t0 = (toCBTree forest3) ^? (ix 1)
    let dt0 = myTreeToDataTree $ cbTreeToBTree $ t0
    putStrLn $ Data.Tree.drawTree (fmap show dt0)
    putStrLn "HS forest (swapped)"
    putStrLn $ Data.Tree.drawTree (fmap show dt)

    -- TODO reindex. (==) defined to ignore position now, so it works.
    print $ f2 == toCBTree forest6

    putStrLn "running test suite..."
    _ <- propertyTests

    tree <- testSpec "hspec tests" testsHspec
    defaultMain (unitTests tree)
