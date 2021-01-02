{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromJust)
import System.Exit (die)
import Data.Set as Set hiding (map, filter, take)
import Control.Monad (join, unless)
import Control.Lens ((^..), has, _Nothing, lengthOf, traversed, filtered)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (annotate, annotateShow, Property, PropertyName, Group(Group), diff, forAll, checkSequential, property, (===))

import Remtrans (number, Tree(..), prog, leaves, findInForest, bitsSet, emptyForest)
import Lib (hremove, toCBTree, CBTree(CBNode, CBEmpty), cuLongToInt, idata, printCB)
import PropertyTests (makeForest)
import Forest (CLeaf)

filterForNode :: Eq a => a -> ([b], Tree a) -> Maybe ()
filterForNode del (_, Node v _ _) | v == del = Just ()
filterForNode _ _ = Nothing

toRemTree :: CBTree a -> Tree a
toRemTree CBEmpty = Empty
toRemTree (CBNode _ _ v a b) = Node v (toRemTree a) (toRemTree b)

findLeaf :: MonadFail m => [Tree CLeaf] -> CLeaf -> m (Int, [Int])
findLeaf remTree cleafToDel =
  let
    f :: ([Int], Tree CLeaf) -> Maybe () = filterForNode cleafToDel
    finder :: [Tree CLeaf] -> Maybe (Int, [Int]) = findInForest f
    found :: Maybe (Int, [Int]) = finder remTree
  in case found of
    Nothing -> fail $ "did not find " ++ show cleafToDel ++ " in " ++ show remTree
    Just x -> return x

prop_sameLeafOrderAfterRemoval :: Property
prop_sameLeafOrderAfterRemoval =
  property $ do
    (toDelete, forest) <- makeForest
    Just refDeleted <- hremove forest toDelete annotate
    let
      intsToDel = map cuLongToInt toDelete
      tree = map toRemTree (toCBTree forest)
      leafData = idata forest
      leavesToDelete = map (leafData !!) intsToDel
      foundLeaves = map (findLeaf tree) leavesToDelete
    paths <- sequenceA foundLeaves
    annotateShow paths
    ourDeleted <- prog tree paths annotate
    annotate $ printCB (toCBTree refDeleted)
    0 === lengthOf
            (traversed
            . leaves
            . filtered (has _Nothing))
            ourDeleted
    (===)
      (map (\x -> toRemTree x ^.. leaves)
           (toCBTree refDeleted))
      (map (\x -> fmap fromJust $ x ^.. leaves)
           ourDeleted)

  
prop_removesNotTooMuchNotTooLittle :: Property
prop_removesNotTooMuchNotTooLittle =
  property $ do
    numLeaves :: Int <- forAll $ Gen.integral (Range.linear 2 100)
    let treeSet = emptyForest numLeaves ()
    let numbered :: [Tree Int] = number treeSet
    let leafVals = join $ map (^.. leaves) numbered
    vals :: [Int] <- forAll $ Gen.shuffle $ leafVals
    numDels :: Int <- forAll $ Gen.integral (Range.linear 1 (numLeaves-1))
    let toDelete = take numDels vals
    let paths = map (\del -> fromJust $ findInForest (filterForNode del) numbered) toDelete
    annotateShow paths
    deleted <- prog numbered paths annotate
    let final :: [Int] = join $ fmap ((^.. leaves).(fmap fromJust)) deleted
    let kept = fromList leafVals `difference` fromList toDelete
    annotateShow toDelete
    annotateShow kept
    diff (         kept)     isSubsetOf (fromList final)
    diff (fromList toDelete) disjoint   (fromList final)

li :: [(PropertyName, Property)]
li = [  ("prop_removesNotTooMuchNotTooLittle", prop_removesNotTooMuchNotTooLittle)
     ,  ("prop_sameLeafOrderAfterRemoval", prop_sameLeafOrderAfterRemoval)
     ]

main :: IO Bool
main = do
  unless (2 == (length $ bitsSet (3 :: Int))) (die "bad bitsSet")
  checkSequential $ Group "property tests" li
