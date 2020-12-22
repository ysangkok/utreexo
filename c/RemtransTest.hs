{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Hedgehog ((===), annotateShow, annotate, Gen)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
--import qualified Hedgehog.Range as Range

import Control.Monad

import Remtrans (number, Tree(..), prog, leavesOnly, findInForest)
import qualified Remtrans

tree :: Gen (Tree ())
tree = Gen.recursive Gen.choice [
    pure Empty
  ] [
    Gen.subterm tree (\subtree -> Node () subtree subtree)
  ]
  

prop_removesNotTooMuch :: Hedgehog.Property
prop_removesNotTooMuch =
  Hedgehog.property $ do
    (treeSet :: Tree ()) <- Hedgehog.forAll tree
    let numbered :: (Tree Int) = number treeSet
    vals :: [Int] <- Hedgehog.forAll $ Gen.shuffle $ leavesOnly numbered
    unless (length vals >= 2) Hedgehog.discard
    let toKeep : toDelete : _ = vals
    unless (toKeep /= toDelete) Hedgehog.discard
    let fun (_,Node v _ _) | v == toDelete = Just ()
        fun _ = Nothing
    let Just path = findInForest fun [numbered]
    annotateShow path
    deleted <- prog [numbered] path annotateShow
    True === (any (toKeep `elem`) deleted)

--prop_removesAskedElem :: Hedgehog.Property
--prop_removesAskedElem =
--  Hedgehog.property $ do
--    undefined

li :: [(Hedgehog.PropertyName, Hedgehog.Property)]
li = [  ("prop_removesNotTooMuch", prop_removesNotTooMuch)
--     ,  ("prop_removesAskedElem",  prop_removesAskedElem)
     ]

main = Hedgehog.checkSequential $ Hedgehog.Group "property tests" li
--main = Remtrans.main
