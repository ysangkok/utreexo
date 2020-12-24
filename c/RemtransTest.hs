{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import System.Exit (die)
import           Hedgehog ((===), annotate, annotateShow, Gen)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Control.Monad
import Data.Set hiding (map)

import Remtrans (number, Tree(..), prog, leavesOnly, findInForest, bitsSet)

tree :: Gen (Tree ())
tree = Gen.recursive Gen.choice [
    pure Empty
  ] [
    Gen.subterm tree (\subtree -> Node () subtree subtree)
  ]
  
deleteTwo :: Hedgehog.PropertyT IO (Int, Int, [Tree (Maybe Int)])
deleteTwo = do
    treeSet :: Set (Tree ()) <- Hedgehog.forAll $ Gen.set (Range.linear 1 100) tree
    let numbered :: [Tree Int] = number $ toList treeSet
    vals :: [Int] <- Hedgehog.forAll $ Gen.shuffle $ join $ map leavesOnly numbered
    unless (length vals >= 2) Hedgehog.discard
    let toKeep : toDelete : _ = vals
    unless (toKeep /= toDelete) Hedgehog.discard
    let fun (_,Node v _ _) | v == toDelete = Just ()
        fun _ = Nothing
    let Just path = findInForest fun numbered
    annotateShow path
    deleted <- prog numbered [path] annotate
    fmap length deleted === fmap (length.(fmap fromJust)) deleted
    return (toKeep, toDelete, deleted)

prop_removesNotTooMuch :: Hedgehog.Property
prop_removesNotTooMuch =
  Hedgehog.property $ do
    (toKeep, _, deleted) <- deleteTwo
    True === (any ((Just toKeep) `elem`) deleted)

prop_removesAskedElem :: Hedgehog.Property
prop_removesAskedElem =
  Hedgehog.property $ do
    (_, toDelete, deleted) <- deleteTwo
    False === (any ((Just toDelete) `elem`) deleted)

li :: [(Hedgehog.PropertyName, Hedgehog.Property)]
li = [  ("prop_removesNotTooMuch", prop_removesNotTooMuch)
     ,  ("prop_removesAskedElem",  prop_removesAskedElem)
     ]

main = do
  unless (2 == (length $ bitsSet (3 :: Int))) (die "bad bitsSet")
  Hedgehog.checkSequential $ Hedgehog.Group "property tests" li
--main = Remtrans.main
