{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (fromJust)
import System.Exit (die)
import Data.Set as Set hiding (map, filter, take)
import Control.Monad (join, unless)
import Control.Lens ((^..))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (annotate, annotateShow, Property, PropertyName, Group(Group), diff, forAll, checkSequential, property)

import Remtrans (number, Tree(..), prog, leaves, findInForest, bitsSet, emptyForest)

filterForNode :: Int -> ([Int], Tree Int) -> Maybe ()
filterForNode del (_, Node v _ _) | v == del = Just ()
filterForNode _ _ = Nothing
  
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
     ]

main :: IO Bool
main = do
  unless (2 == (length $ bitsSet (3 :: Int))) (die "bad bitsSet")
  checkSequential $ Group "property tests" li
