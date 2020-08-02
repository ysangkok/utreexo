{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module PropertyTests (propertyTests) where

import Forest (CLeaf(CLeaf))
import Lib
import           Hedgehog ((===), annotateShow, annotate)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Bits ((.|.), shiftL)
import Data.Binary (encode, decode)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.WideWord.Word128 (Word128)
import Data.Word (Word64, Word8)
import Foreign.C (CULong)

prop_iso :: Hedgehog.Property
prop_iso =
  Hedgehog.property $ do
    let vals1 = Gen.word64 $ Range.constantFrom 1 0 maxBound
    let vals2 = Gen.word64 $ Range.constantFrom 1 0 maxBound
    v <- Hedgehog.forAll vals1
    w <- Hedgehog.forAll vals2
    let x :: Word128 = ((word64ToWord128 v) `shiftL` 64) .|. (word64ToWord128 w)
    x === (decode $ encode x)

prop_hashRow :: Hedgehog.Property
prop_hashRow =
  Hedgehog.property $ do
    let vals = Gen.set (Range.constantFrom 10 10 1000) (Gen.word64 $ Range.constantFrom 0 0 1000)
    xs <- Hedgehog.forAll vals

    let mforest = forestWithLeaves [CLeaf (word64ToWord128 x) 0 | x <- Set.toList xs]
    case mforest of
      Nothing -> Hedgehog.discard
      _ -> return ()
    let forest = fromMaybe (error "impossible with discard above") mforest
    annotateShow $ ipositions forest
    let inserted = intToWord64 $ length xs - 1
    let dirt = Gen.set (Range.constantFrom 1 1 100) (Gen.word64 $ Range.constantFrom inserted inserted (2 ^ (ccharToInteger $ rows forest) - 1))
    dirtSet <- Hedgehog.forAll dirt
    let dirtList :: [CULong] = map word64ToCULong $ Set.toList dirtSet
    let word8Rows :: Word8 = ccharToWord8 $ rows forest
    let filtered = [x | x <- dirtList
                        , let chld = intToWord64 $ child x (rows forest) .|. 1
                        , let numData = inumleaves forest
                        , inForest chld numData (word8ToInt word8Rows)
                        ]
    if length filtered == 0
        then Hedgehog.discard
        else return ()
    annotate $ printTree forest
    annotateShow $ ipositions forest
    let ref = hashRow forest filtered
    case fmap printTree ref of
      Nothing -> return ()
      Just x -> annotate x
    annotateShow $ fmap ipositions ref
    let our = hhashRow forest filtered
    toRecord ref === toRecord our


prop_swapNodes :: Hedgehog.Property
prop_swapNodes =
  Hedgehog.property $ do
    let vals = Gen.set (Range.constantFrom 3 3 99) (Gen.word64 $ Range.constantFrom 1 1 99)
    xs <- Hedgehog.forAll vals
    let Just forest = forestWithLeaves [sha256 x | x <- Set.toList xs]
    annotateShow $ rows forest
    annotate $ printTree forest
    let fromGen = Gen.integral $ Range.constantFrom 1 0 (intToCULong $ 2 ^ (rows forest) - 1)
    let toGen = Gen.integral $ Range.constantFrom 1 0 (intToCULong $ 2 ^ (rows forest) - 1)
    (to :: CULong) <- Hedgehog.forAll toGen
    (from :: CULong) <- Hedgehog.forAll fromGen
    let rows64 = ccharToWord8 $ rows forest
    let fromRow = word8ToCChar $ detectRow (cuLongToWord64 from) rows64
    let   toRow = word8ToCChar $ detectRow (cuLongToWord64 to) rows64
    if fromRow /= toRow
        then Hedgehog.discard
        else return ()
    let row = fromRow
    if to == from then Hedgehog.discard else return ()
    let fromInt = cuLongToInt from
    let   toInt = cuLongToInt to
    if (idata forest !! fromInt) == CLeaf 0 0 then Hedgehog.discard else return ()
    if (idata forest !!   toInt) == CLeaf 0 0 then Hedgehog.discard else return ()
    annotateShow (from, to)
    let ref = swapNodes forest from to row
    --case ref of
    --  Just x -> do annotate $ printTree x
    --               annotateShow $ idata x
    --  _ -> return ()
    let our = hswapNodes forest from to row
    --annotateShow $ numLeaves forest
    --annotateShow $ length $ ipositions forest
    --annotateShow $ length $ idata forest
    toRecord ref === toRecord our

prop_delete :: Hedgehog.Property
prop_delete =
  Hedgehog.property $ do
    let lower :: Word64 = 2 ^ (31 :: Integer)
    let upper :: Word64 = 2 ^ (32 :: Integer) - 1
    let vals = Gen.set (Range.constantFrom 5 100 100) (Gen.word64 $ Range.constantFrom lower lower upper)
    xs <- Hedgehog.forAll vals

    let mforest = forestWithLeaves [CLeaf (word64ToWord128 x) 0 | x <- Set.toList xs]
    case mforest of
      Nothing -> Hedgehog.discard
      _ -> return ()
    let forest = fromMaybe (error "impossible with discard above") mforest
    annotateShow $ ipositions forest
    let inserted = intToWord64 $ length xs - 1
    -- ToC makes tree be Nothing if emptied. So don't empty it:
    let dirt = Gen.set (Range.constantFrom 1 1 (length xs - 1)) (Gen.word64 $ Range.constantFrom 0 0 inserted)
    dirtSet <- Hedgehog.forAll dirt
    let dirtList :: [CULong] = map word64ToCULong $ Set.toList dirtSet
    let ref = deleteFromForest forest dirtList
    let our = hremove          forest dirtList
    toRecord ref === toRecord our


li :: [(Hedgehog.PropertyName, Hedgehog.Property)]
li = [  ("prop_swapNodes", prop_swapNodes)
      , ("prop_iso", prop_iso)
      , ("prop_hashRow", prop_hashRow)
      , ("prop_delete", prop_delete)
     ]

propertyTests :: IO Bool
propertyTests = Hedgehog.checkSequential $ Hedgehog.Group "property tests" li
