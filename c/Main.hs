{-# LANGUAGE ForeignFunctionInterface, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}

import           Hedgehog ((===), annotateShow, annotate)
import qualified Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.WideWord.Word128 (Word128(Word128), byteSwapWord128, word128Hi64, word128Lo64, showHexWord128)

import qualified Crypto.Hash.SHA256 as SHA256
import System.IO.Unsafe (unsafePerformIO)
import Forest
import Foreign.Ptr (nullPtr, FunPtr, plusPtr, castPtr)
import Foreign (Ptr, peek, withArrayLen, peekArray, ForeignPtr, newForeignPtr, withForeignPtr, poke, pokeArray)
import Foreign.C (CSize(CSize), CULong(CULong), CChar(CChar))
import Foreign.Marshal.Alloc (free, alloca)
import Foreign.C.String (peekCString)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hspec (Spec, shouldBe, testSpec, runIO, it)

import qualified Data.ByteString.Base16.Lazy as B16
import Data.Binary (Binary(put,get), encode, decode)
import Data.ByteString.Lazy (toStrict, ByteString)
import Data.ByteString.Lazy.Char8 (unpack, pack)
import qualified Data.ByteString.Lazy as BS
import Data.Map (toList, fromList, adjust, Map, difference)
import Data.Function((&))
import Control.Lens.Operators ((.~), (%~)) -- <&>
import Control.Lens.At (at, ix)
import Control.Lens.Combinators (Lens', lens) -- both, _1
import Data.Either (fromRight)
import Data.Functor.Identity (Identity)
import Data.Word (Word64, Word8, byteSwap64)
import Data.List (inits, sort, sortOn, elemIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Either (lefts, rights)
import Data.Bits
import qualified Data.Set as Set
import qualified Data.Tree (Tree(Node), unfoldTree, drawForest, Forest)

import Control.Zipper (fromWithin, rezip, zipper, focus, Top, (:>>), Zipper) -- downward
import Data.Tuple (swap)
--import Control.Lens.Plated (plate)

import Control.Monad.State.Lazy (runState, modify, lift, State)
import qualified Control.Monad.State.Lazy as State (get, put)
import Pipes.Prelude (toListM)
import Pipes (yield, Producer)
import Control.Monad (forM_, unless, mzero)
import Control.Monad.Loops (whileM_)

import Debug.Trace
import Data.Aeson (eitherDecodeFileStrict', Array, FromJSON(..), ToJSON(..), genericToJSON, genericParseJSON)
import Data.Aeson.Casing
import GHC.Generics

foreign import ccall "libutreexo.h"
    cForestPrint
    :: Ptr CForest
    -> IO (Ptr CChar)

foreign import ccall "libutreexo.h"
    cForestAdd
    :: Ptr CForest
    -> Ptr CLeaf
    -> CSize
    -> IO (Ptr CForest) -- can be null!

foreign import ccall "libutreexo.h"
    cForestDelete
    :: Ptr CForest
    -> Ptr CULong
    -> CSize
    -> IO (Ptr CForest) -- can be null!

foreign import ccall "libutreexo.h"
    cForestPrepareInsertion
    :: Ptr CForest
    -> CULong
    -> IO (Ptr CForest)

foreign import ccall "libutreexo.h"
    cForestSwapNodes
    :: Ptr CForest
    -> CULong
    -> CULong
    -> CChar
    -> IO (Ptr CForest)

foreign import ccall "libutreexo.h"
    cForestHashRow
    :: Ptr CForest
    -> Ptr CULong
    -> CSize
    -> IO (Ptr CForest)

foreign import ccall "libutreexo.h &cForestFree"
  cForestFree :: FunPtr (Ptr CForest -> IO ())

parentMany :: Word64 -> Int -> Int -> Word64
parentMany position rise forestRows =
        -- 18       6    2
        -- example of underflowing innerMask above
  let
    mask :: Word64
    mask = if forestRows < 1 then error "non-positive forestRows"
                             else 2 `shiftL` forestRows - 1
    innerMask :: Int
    innerMask = if rise > forestRows then error "parentMany rise > forestRows"
                                     else forestRows - rise + 1
  in
    if rise == 0 then
        position
    else
        mask .&.
            (position `shiftR` rise .|. mask `shiftL` innerMask)

getRootsReverse :: Word64 -> Int -> ([Word64], [Int])
getRootsReverse leaves forestRows =
  let
    rows = [row | row <- [forestRows, forestRows - 1 .. 0], 1 `shiftL` row .&. leaves /= 0]
    positionIncrements = inits $ map (\row -> 1 `shiftL` row) rows
    positions = map sum positionIncrements
    positionsAndRows = zip positions rows
    mkRoot (position, row) = parentMany position row forestRows
    roots = map mkRoot positionsAndRows
  in
    (reverse roots, reverse rows)

tests :: TestTree
tests = testGroup "parentMany/getRootsReverse tests"
  [
    testCase "parentMany(position=0, rise=row=3, forestRows=5)=56" $
     parentMany 0 3 5 @?= 56
  , testCase "parentMany(position=8, rise=row=2, forestRows=5)=50" $
     parentMany 8 2 5 @?= 50
  , testCase "parentMany(position=0, rise=row=4, forestRows=5)=60" $
     parentMany 0 4 5 @?= 60
  , testCase "parentMany(position=16, rise=row=2, forestRows=5)=52" $
     parentMany 16 2 5 @?= 52
  , testCase "parentMany(position=20, rise=row=1, forestRows=5)=42" $
     parentMany 20 1 5 @?= 42
  , testCase "parentMany(position=22, rise=row=0, forestRows=5)=22" $
     parentMany 22 0 5 @?= 22
  , testCase "parentMany(position=0, rise=row=0, forestRows=5)=0" $
     parentMany 0 0 5 @?= 0
  , testCase "getRootsReverse1" $
     getRootsReverse 23 5 @?= ([22, 42, 52, 60], [0, 1, 2, 4])
  , testCase "getRootsReverse2" $
     getRootsReverse 22 5 @?= ([42, 52, 60], [1, 2, 4])
  , testCase "getRootsReverse3" $
     getRootsReverse 21 5 @?= ([20, 52, 60], [0, 2, 4])
  , testCase "getRootsReverse4" $
     getRootsReverse 20 5 @?= ([52, 60], [2, 4])
  , testCase "getRootsReverse4" $
     getRootsReverse 19 5 @?= ([18, 40, 60], [0, 1, 4])
  , testCase "getRootsReverse5" $
     getRootsReverse 18 5 @?= ([40, 60], [1, 4])
  , testCase "getRootsReverse6" $
     getRootsReverse 17 5 @?= ([16, 60], [0, 4])
  , testCase "getRootsReverse7" $
     getRootsReverse 16 5 @?= ([60], [4])
  , testCase "getRootsReverse 15 5" $
     getRootsReverse 15 5 @?= ([14, 38, 50, 56], [0, 1, 2, 3])
  , testCase "getRootsReverse 3 5" $
     getRootsReverse 3 5 @?= ([2, 32], [0, 1])
  , testCase "getRootsReverse 2 5" $
     getRootsReverse 2 5 @?= ([32], [1])
  , testCase "getRootsReverse 1 5" $
     getRootsReverse 1 5 @?= ([0], [0])
  , testCase "getRootsReverse 0 5" $
     getRootsReverse 0 5 @?= ([], [])
  ]

class IForest a where
    ipositions :: a -> Map Word128 Word64
    idata :: a -> [CLeaf]
    inumleaves :: a -> Word64
    irows :: a -> CChar
    itohforest :: a -> HForest

toRecord :: (IForest a) => Maybe a -> Maybe (Map Word128 Word64, [CLeaf], Word64)
toRecord = fmap (\a -> (ipositions a, idata a, inumleaves a))

instance IForest Forest where
    itohforest a = HForest (ipositions a) (idata a) (inumleaves a) (irows a)
    irows = rows
    ipositions (Forest forestForeignPtr) =
        let
          miniposses =
              unsafePerformIO $
                  withForeignPtr forestForeignPtr $ \forestPtr -> do
                      peeked_forest <- peek forestPtr
                      let numpos = word64ToInt $ num_leaves peeked_forest
                      peekArray numpos $ position_map peeked_forest
        in
          fromList [(mini x, pos x) | x <- miniposses] -- map (\(CMiniPos mini pos) -> CMiniPos (mini `shiftR` 32) pos) miniposses
    idata (Forest forestForeignPtr) =
        unsafePerformIO $
            withForeignPtr forestForeignPtr $ \forestPtr -> do
                peeked_forest <- peek forestPtr
                let numleaves = word64ToInt $ leaves_size peeked_forest
                peekArray numleaves $ leaves peeked_forest
    inumleaves (Forest fptr) = unsafePerformIO $ do
      withForeignPtr fptr $ \for -> do
        f <- peek for
        return $ num_leaves f

printTree :: Forest -> String
printTree (Forest forestForeignPtr) = unsafePerformIO $ do
    withForeignPtr forestForeignPtr $ \forestPtr -> do
        charPtr <- cForestPrint forestPtr
        peeked_string <- peekCString charPtr
        free charPtr
        return $ peeked_string

data Forest = Forest (ForeignPtr CForest)

cToHForest :: (Ptr CForest) -> [CLeaf] -> IO (Maybe Forest)
cToHForest oldForestPtr [] = return Nothing
cToHForest oldForestPtr leaves =
    withArrayLen leaves $ \leavesLen leavesPtr -> do
        let cSize = intToCSize leavesLen
        forestPtr <- cForestAdd oldForestPtr leavesPtr cSize
        checkNull forestPtr

deleteFromForest :: Forest -> [CULong] -> Maybe Forest
deleteFromForest forest [] = Just forest
deleteFromForest (Forest forestForeignPtr) positions =
    unsafePerformIO $
        withForeignPtr forestForeignPtr $ \oldForestPtr ->
            withArrayLen positions $ \positionsLen positionsPtr -> do
                let cSize = intToCSize positionsLen
                forestPtr <- cForestDelete oldForestPtr positionsPtr cSize
                checkNull forestPtr

checkNull :: Ptr CForest -> IO (Maybe Forest)
checkNull forestPtr =
    if forestPtr == nullPtr
        then return Nothing
        else do
            fp <- newForeignPtr cForestFree forestPtr
            return $ Just $ Forest fp

forestWithLeaves :: [CLeaf] -> Maybe Forest
forestWithLeaves leaves =
    unsafePerformIO $
        cToHForest nullPtr leaves

addToForest :: Forest -> [CLeaf] -> Maybe Forest
addToForest forest [] = Just forest
addToForest (Forest forestForeignPtr) leaves =
    unsafePerformIO $
        withForeignPtr forestForeignPtr $ \oldForestPtr ->
            cToHForest oldForestPtr leaves

prepareInsertion :: Forest -> CULong -> Maybe Forest
prepareInsertion (Forest forestForeignPtr) delta =
    unsafePerformIO $
        withForeignPtr forestForeignPtr $ \forestPtr -> do
            newPtr <- cForestPrepareInsertion forestPtr delta
            checkNull newPtr

swapNodes :: Forest -> CULong -> CULong -> CChar -> Maybe Forest
swapNodes (Forest forestForeignPtr) from to row =
    unsafePerformIO $
        withForeignPtr forestForeignPtr $ \forestPtr -> do
            newPtr <- cForestSwapNodes forestPtr from to row
            checkNull newPtr

hashRow :: Forest -> [CULong] -> Maybe Forest
hashRow (Forest ffptr) dirt =
    unsafePerformIO $
        withForeignPtr ffptr $ \fptr ->
            withArrayLen dirt $ \len pptr -> do
                let csizelen = intToCSize len
                newPtr <- cForestHashRow fptr pptr csizelen
                checkNull newPtr

data HForest = HForest {
    hpositions :: Map Word128 Word64
  , hdata :: [CLeaf]
  , hnumleaves :: Word64
  , hrows :: CChar
} deriving (Eq, Show)

instance IForest HForest where
    ipositions = hpositions
    idata = hdata
    inumleaves = hnumleaves
    irows = hrows

child :: CULong -> CChar -> Int
child pos forestRowsChar =
   word64ToInt $ childMany pos 1 forestRowsChar

childMany :: CULong -> CChar -> CChar -> Word64
childMany position dropChar forestRowsChar =
  let
    forestRows = ccharToInt forestRowsChar
    drop = ccharToInt dropChar
    mask = (2 `shiftL` forestRows) - 1
  in
    cuLongToWord64 $ (position `shiftL` drop) .&. mask

rows :: Forest -> CChar
rows (Forest ptr) =
    unsafePerformIO $ withForeignPtr ptr $ \x -> do
        cforest <- peek x
        let h = height cforest
        let converted = word8ToCChar h
        return converted

instance Data.Binary.Binary Word128 where
  put (Word128 a64 b64) = put a64 <> put b64
  get = do
          (a1 :: Word64) <- get
          (a2 :: Word64) <- get
          return $ Word128 { word128Hi64 = a1, word128Lo64 = a2 }

firstTwelveBytes :: CLeaf -> Word128
firstTwelveBytes leaf =
    let twelve :: ByteString = BS.take 12 $ encode $ byteSwapWord128 $ first leaf
        padding :: ByteString = BS.replicate 4 0
    in byteSwapWord128 $ decode $ twelve <> padding

instance Show CLeaf where
    show (CLeaf first second) = "CLeaf { " ++ showHexWord128 (byteSwapWord128 first) ++ " " ++ showHexWord128 (byteSwapWord128 second) ++ " }"

swapTwo f s xs = zipWith (\x y ->
    if x == f then xs !! s
    else if x == s then xs !! f
    else y) [0..] xs

hswapNodes :: IForest a => a -> CULong -> CULong -> CChar -> Maybe HForest
hswapNodes goforest from to row =
    let
        posi = ipositions goforest
        leaf = idata goforest
        a = childMany from row (irows goforest)
        b = childMany to   row (irows goforest)
        tempmap = snd $ flip runState posi $ do
            let run = 1 `shiftL` (ccharToInt row)
            forM_ [0..run-1] $ \i -> do
                let lA = leaf !! (word64ToInt $ a+i)
                let lB = leaf !! (word64ToInt $ b+i)
                let cA = firstTwelveBytes lA
                let cB = firstTwelveBytes lB
                (gotten :: Map Word128 Word64) <- State.get
                State.put $ gotten & at cB .~ Just (a+i)
                                   & at cA .~ Just (b+i)
        newmap = tempmap
        bottomup = snd $ flip runState (leaf, a, b) $ do
            forM_ [0..row] $ \r -> do
                (before, ia, ib) <- State.get
                let run = 1 `shiftL` (ccharToInt $ row - r)
                    after = snd $ flip runState before $
                            forM_ (zip [ia..ia+run-1] [ib..ib+run-1]) $ \(x, y) -> do
                                ibefore <- State.get
                                let intx = word64ToInt x
                                    inty = word64ToInt y
                                    e1 = swapTwo intx inty ibefore
                                State.put e1
                    charrows = ccharToInt $ irows goforest
                State.put (after, parent ia charrows, parent ib charrows)
        (newleaf, _, _) = bottomup
    in
        if row == 0
            then
              let
                f = cuLongToInt from
                t = cuLongToInt to
                row0leaf = swapTwo f t leaf
                -- now data has been swapped, swap positions:
                f2 = cuLongToWord64 from
                t2 = cuLongToWord64 to
                lA = row0leaf !! f
                lB = row0leaf !! t
                row0map = posi   & at (firstTwelveBytes lA) .~ Just f2
                                 & at (firstTwelveBytes lB) .~ Just t2
              in Just $ HForest row0map row0leaf (inumleaves goforest) (irows goforest)
            else if a == b
                then Nothing
                else Just $ HForest newmap newleaf (inumleaves goforest) (irows goforest)


testLeaves :: [CLeaf]
testLeaves = [CLeaf 0x10 0, CLeaf 0x20 0, CLeaf 0x30 0]

testLeaves2 :: [CLeaf]
testLeaves2 = [CLeaf 0x15 0, CLeaf 0x25 0, CLeaf 0x35 0]

testLeaves3 :: [CLeaf]
testLeaves3 = [CLeaf i 0 | i <- [1..100]]

parent :: Word64 -> Int -> Word64
parent position forestRows =
  position `shiftR` 1 .|. 1 `shiftL` forestRows

extractTwins :: [Word64] -> Int -> ([Word64], [Word64])
extractTwins nodes row =
  let
    pairs    = zip <*> tail $ nodes
    matching = [ (x,y) | (x,y) <- pairs, x .|. 1 == y ]
    firsts   = map fst matching
    both     = firsts ++ map snd matching
    parents  = map (flip parent row) firsts
    dels     = [ x | x <- nodes, not $ x `elem` both ]
  in
    (parents, dels)


tests2 :: TestTree
tests2 = testGroup "extractTwins tests"
  [
     testCase "  1" $ extractTwins [0, 1, 2, 3, 4, 7, 10, 13, 14, 20, 22, 23] 5 @?= ([32, 33, 43], [4, 7, 10, 13, 14, 20])
   , testCase "  2" $ extractTwins [0, 1, 2, 5, 6, 7, 14, 18, 21, 22, 23, 24, 25] 5 @?= ([32, 35, 43, 44], [2, 5, 14, 18, 21])
   , testCase "  3" $ extractTwins [0, 1, 3, 5, 7, 8, 10, 12, 13] 4 @?= ([16, 22], [3, 5, 7, 8, 10])
   , testCase "  4" $ extractTwins [0] 2 @?= ([], [0])
   , testCase "  5" $ extractTwins [0, 3, 5, 7, 8, 10, 11, 12, 13, 14, 16, 21, 22, 24] 5 @?= ([37, 38], [0, 3, 5, 7, 8, 14, 16, 21, 22, 24])
   , testCase "  6" $ extractTwins [0, 5, 6, 7, 8, 10, 11, 14, 15, 16, 17, 18, 21] 5 @?= ([35, 37, 39, 40], [0, 5, 8, 18, 21])
   , testCase "  7" $ extractTwins [114, 115] 6 @?= ([121], [])
   , testCase "  8" $ extractTwins [121] 6 @?= ([], [121])
   , testCase "  9" $ extractTwins [1, 2, 4, 7, 11, 12, 15, 19] 5 @?= ([], [1, 2, 4, 7, 11, 12, 15, 19])
   , testCase " 10" $ extractTwins [1] 4 @?= ([], [1])
   , testCase " 11" $ extractTwins [16, 18, 20, 21] 4 @?= ([26], [16, 18])
   , testCase " 12" $ extractTwins [16] 4 @?= ([], [16])
   , testCase " 13" $ extractTwins [] 2 @?= ([], [])
   , testCase " 14" $ extractTwins [24] 4 @?= ([], [24])
   , testCase " 15" $ extractTwins [25] 4 @?= ([], [25])
   , testCase " 16" $ extractTwins [32, 33, 35, 38, 42, 43] 5 @?= ([48, 53], [35, 38])
   , testCase " 17" $ extractTwins [32, 34, 35, 41, 42, 43] 5 @?= ([49, 53], [32, 41])
   , testCase " 18" $ extractTwins [33, 35, 37, 38, 39, 42] 5 @?= ([51], [33, 35, 37, 42])
   , testCase " 19" $ extractTwins [33, 35, 38, 41] 5 @?= ([], [33, 35, 38, 41])
   , testCase " 20" $ extractTwins [34, 35, 37, 39, 40, 41] 5 @?= ([49, 52], [37, 39])
   , testCase " 21" $ extractTwins [] 4 @?= ([], [])
   , testCase " 22" $ extractTwins [48, 51, 53] 5 @?= ([], [48, 51, 53])
   , testCase " 23" $ extractTwins [49, 51, 53] 5 @?= ([], [49, 51, 53])
   , testCase " 24" $ extractTwins [49, 51] 5 @?= ([], [49, 51])
   , testCase " 25" $ extractTwins [49, 52, 53] 5 @?= ([58], [49])
   , testCase " 26" $ extractTwins [49] 5 @?= ([], [49])
   , testCase " 27" $ extractTwins [] 5 @?= ([], [])
   , testCase " 28" $ extractTwins [56] 5 @?= ([], [56])
   , testCase " 29" $ extractTwins [57] 5 @?= ([], [57])
   , testCase " 30" $ extractTwins [] 6 @?= ([], [])
   , testCase " 31" $ extractTwins [68, 69, 72, 74, 76, 77, 80, 81] 6 @?= ([98, 102, 104], [72, 74])
   , testCase " 32" $ extractTwins [6, 8, 9, 10, 16, 17, 18, 20, 24, 25, 26, 27, 30, 32, 33, 34, 35, 37] 6 @?= ([68, 72, 76, 77, 80, 81], [6, 10, 18, 20, 30, 37])
   , testCase " 33" $ extractTwins [98, 101, 102] 6 @?= ([], [98, 101, 102])
  ]

swapIfDescendant :: (Word64, Word64) -> (Word64, Word64) -> Int -> Int -> Int -> Word64
swapIfDescendant (a_from, a_to) (b_from, b_to) ar br forestRows =
  let
      hdiff = ar - br
      bup = parentMany b_to hdiff forestRows
      rootMask = a_from `xor` a_to
  in
      if (bup == a_from) /= (bup == a_to)
          then rootMask `shiftL` hdiff
          else 0

tests3 :: TestTree
tests3 = testGroup "swapIfDescendant tests"
  [
     testCase "1" $ swapIfDescendant (56, 56) (52, 50) 3 2 5 @?= 0
   , testCase "2" $ swapIfDescendant (57, 56) (22, 12) 3 0 5 @?= 8
   , testCase "3" $ swapIfDescendant (57, 56) (48, 50) 3 2 5 @?= 2
   , testCase "4" $ swapIfDescendant (48, 48) (22, 4) 2 0 5 @?= 0
   , testCase "5" $ swapIfDescendant (24, 24) (8, 6) 2 0 4 @?= 0
   , testCase "6" $ swapIfDescendant (24, 24) (18, 18) 2 1 4 @?= 0
   , testCase "7" $ swapIfDescendant (18, 18) (8, 6) 1 0 4 @?= 0
   , testCase "9" $ swapIfDescendant (56, 56) (21, 14) 3 0 5 @?= 0
   , testCase "a" $ swapIfDescendant (56, 56) (40, 38) 3 1 5 @?= 0
   , testCase "b" $ swapIfDescendant (56, 56) (51, 50) 3 2 5 @?= 0
   , testCase "c" $ swapIfDescendant (51, 50) (21, 14) 2 0 5 @?= 4
   , testCase "d" $ swapIfDescendant (51, 50) (40, 38) 2 1 5 @?= 2
   , testCase "e" $ swapIfDescendant (40, 36) (21, 4) 1 0 5 @?= 0
   , testCase "f" $ swapIfDescendant (56, 56) (20, 8) 3 0 5 @?= 0
   , testCase "g" $ swapIfDescendant (56, 56) (52, 50) 3 2 5 @?= 0
   , testCase "h" $ swapIfDescendant (120, 120) (101, 100) 4 2 6 @?= 0
   , testCase "i" $ swapIfDescendant (24, 24) (12, 4) 2 0 4 @?= 0
   , testCase "k" $ swapIfDescendant (52, 49) (24, 16) 2 0 5 @?= 20
  ]

swapInRow :: (Word64, Word64) -> [Maybe (Word64, Word64)] -> Int -> [Maybe (Word64, Word64)]
swapInRow s collapses r =
    let
      forestRows = length collapses
      updateTo idx (from, to) =
          (from, to `xor` mask)
        where
          mask = swapIfDescendant s (from, to) r idx forestRows
      fun (maybeSwap, idx) =
          if idx < r then fmap (updateTo idx) maybeSwap
                     else maybeSwap
    in
      fmap fun (zip collapses [0..])

tests4 :: TestTree
tests4 = testGroup "swapInRow tests"
  [
      testCase "0" $ swapInRow (51, 49) [Just (27, 14), Just (46, 38), Just (52, 50), Just (56, 56), Nothing] 2 @?= [Just (27, 6), Just (46, 34), Just (52, 50), Just (56, 56), Nothing]
    , testCase "1" $ swapInRow (19, 16) [Just (13, 6), Just (20, 18), Just (24, 24), Nothing] 1 @?= [Just (13, 0), Just (20, 18), Just (24, 24), Nothing]
    , testCase "2" $ swapInRow (114, 113) [Just (32, 20), Nothing, Just (105, 100), Nothing, Just (120, 120), Nothing] 3 @?= [Just (32, 12), Nothing, Just (105, 98), Nothing, Just (120, 120), Nothing]
    , testCase "3" $ swapInRow (99, 97) [Just (32, 12), Nothing, Just (105, 98), Nothing, Just (120, 120), Nothing] 2 @?= [Just (32, 4), Nothing, Just (105, 98), Nothing, Just (120, 120), Nothing]
    , testCase "4" $ swapInRow (114, 113) [Nothing, Just (80, 74), Just (105, 100), Nothing, Just (120, 120), Nothing] 3 @?= [Nothing, Just (80, 70), Just (105, 98), Nothing, Just (120, 120), Nothing]
    , testCase "5" $ swapInRow (39, 37) [Just (26, 14), Just (44, 38), Just (52, 50), Just (56, 56), Nothing] 1 @?= [Just (26, 10), Just (44, 38), Just (52, 50), Just (56, 56), Nothing]
    , testCase "6" $ swapInRow (101, 98) [Nothing, Just (84, 74), Just (102, 100), Nothing, Just (120, 120), Nothing] 2 @?= [Nothing, Just (84, 68), Just (102, 100), Nothing, Just (120, 120), Nothing]
    , testCase "7" $ swapInRow (50, 49) [Nothing, Just (40, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Nothing, Just (40, 34), Nothing, Just (56, 56), Nothing]
    , testCase "8" $ swapInRow (51, 48) [Nothing, Just (45, 38), Just (52, 50), Just (56, 56), Nothing] 2 @?= [Nothing, Just (45, 32), Just (52, 50), Just (56, 56), Nothing]
    , testCase "9" $ swapInRow (50, 49) [Nothing, Just (42, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Nothing, Just (42, 34), Nothing, Just (56, 56), Nothing]
    , testCase "a" $ swapInRow (11, 10) [Just (4, 6), Just (11, 10), Just (12, 12)] 1 @?= [Just (4, 4), Just (11, 10), Just (12, 12)]
    , testCase "b" $ swapInRow (50, 49) [Just (16, 10), Just (43, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Just (16, 6), Just (43, 34), Nothing, Just (56, 56), Nothing]
    , testCase "c" $ swapInRow (57, 56) [Nothing, Nothing, Just (48, 50), Just (57, 56), Nothing] 3 @?= [Nothing, Nothing, Just (48, 48), Just (57, 56), Nothing]
    , testCase "d" $ swapInRow (18, 17) [Just (9, 4), Nothing, Just (24, 24), Nothing] 1 @?= [Just (9, 2), Nothing, Just (24, 24), Nothing]
    , testCase "e" $ swapInRow (101, 97) [Nothing, Just (82, 74), Just (104, 100), Nothing, Just (120, 120), Nothing] 2 @?= [Nothing, Just (82, 66), Just (104, 100), Nothing, Just (120, 120), Nothing]
    , testCase "f" $ swapInRow (51, 50) [Nothing, Just (43, 38), Just (51, 50), Just (56, 56), Nothing] 2 @?= [Nothing, Just (43, 36), Just (51, 50), Just (56, 56), Nothing]
    , testCase "g" $ swapInRow (50, 49) [Just (24, 10), Just (42, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Just (24, 6), Just (42, 34), Nothing, Just (56, 56), Nothing]
    , testCase "h" $ swapInRow (35, 33) [Just (24, 6), Just (42, 34), Nothing, Just (56, 56), Nothing] 1 @?= [Just (24, 2), Just (42, 34), Nothing, Just (56, 56), Nothing]
    , testCase "i" $ swapInRow (41, 39) [Just (27, 18), Just (44, 40), Nothing, Nothing, Nothing] 1 @?= [Just (27, 14), Just (44, 40), Nothing, Nothing, Nothing]
    , testCase "j" $ swapInRow (50, 49) [Nothing, Just (38, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Nothing, Just (38, 34), Nothing, Just (56, 56), Nothing]
    , testCase "k" $ swapInRow (20, 17) [Just (14, 8), Nothing, Nothing, Just (28, 28)] 1 @?= [Just (14, 2), Nothing, Nothing, Just (28, 28)]
    , testCase "l" $ swapInRow (101, 97) [Nothing, Just (84, 74), Just (103, 100), Nothing, Just (120, 120), Nothing] 2 @?= [Nothing, Just (84, 66), Just (103, 100), Nothing, Just (120, 120), Nothing]
    , testCase "m" $ swapInRow (50, 49) [Nothing, Just (42, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Nothing, Just (42, 34), Nothing, Just (56, 56), Nothing]
    , testCase "n" $ swapInRow (50, 49) [Just (17, 10), Just (38, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Just (17, 6), Just (38, 34), Nothing, Just (56, 56), Nothing]
    , testCase "o" $ swapInRow (58, 57) [Just (24, 16), Nothing, Nothing, Nothing, Nothing] 3 @?= [Just (24, 8), Nothing, Nothing, Nothing, Nothing]
    , testCase "p" $ swapInRow (50, 49) [Just (24, 8), Nothing, Nothing, Nothing, Nothing] 2 @?= [Just (24, 4), Nothing, Nothing, Nothing, Nothing]
  ]

swapCollapses :: [[(Word64, Word64)]] -> [Maybe (Word64, Word64)] -> [Maybe (Word64, Word64)]
swapCollapses swaps collapses = snd $ flip runState collapses $ do
    forM_ [length collapses - 1, length collapses - 2 .. 1] $ \r -> do
      forM_ (swaps !! r) $ \s ->
        modify (\coll -> swapInRow s coll r) -- this would take forestRows
      val <- State.get
      case val !! r of
        Just rowcol -> modify (\coll -> swapInRow rowcol coll r)
        Nothing -> return ()

tests5 :: TestTree
tests5 = testGroup "swapCollapses tests"
  [
    testCase "0" $ swapCollapses [[(5, 3), (17, 12)], [(41, 32)], [], [], []] [Nothing, Nothing, Just (48, 50), Just (57, 56), Nothing] @?= [Nothing, Nothing, Just (48, 48), Just (57, 56), Nothing]
    , testCase "1" $ swapCollapses [[(7, 1), (12, 9)], [(20, 17)], [], []] [Nothing, Nothing, Just (24, 24), Nothing] @?= [Nothing, Nothing, Just (24, 24), Nothing]
    , testCase "2" $ swapCollapses [[], [], [], [], [], []] [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing] @?= [Nothing,Nothing,Nothing,Nothing,Nothing,Nothing]
    , testCase "3" $ swapCollapses [[(13, 9), (18, 15), (24, 22), (28, 26), (36, 31)], [(72, 70), (79, 76)], [(102, 100)], [], [], []] [Nothing, Nothing, Nothing, Just (114, 114), Just (120, 120), Nothing] @?= [Nothing, Nothing, Nothing, Just (114, 114), Just (120, 120), Nothing]
  ]

rootPosition :: Word64 -> Int -> Int -> Word64
rootPosition leaves h forestRows =
  let
    mask = 2 `shiftL` forestRows - 1
    before = leaves .&. (mask `shiftL` (h + 1))
    shifted = (before `shiftR` h) .|. (mask `shiftL` (forestRows - (h - 1)))
  in
    shifted .&. mask

tests6 :: TestTree
tests6 = testGroup "rootPosition tests"
  [
      testCase "1" $ rootPosition 5 0 3 @?= 4
    , testCase "2" $ rootPosition 5 1 3 @?= 10
    , testCase "3" $ rootPosition 5 2 3 @?= 12
    , testCase "4" $ rootPosition 29 0 5 @?= 28
    , testCase "5" $ rootPosition 29 1 5 @?= 46
    , testCase "6" $ rootPosition 29 2 5 @?= 54
    , testCase "7" $ rootPosition 29 3 5 @?= 58
    , testCase "8" $ rootPosition 29 4 5 @?= 60
    , testCase "9" $ rootPosition 31 0 5 @?= 30
    , testCase "a" $ rootPosition 31 1 5 @?= 46
    , testCase "b" $ rootPosition 31 2 5 @?= 54
    , testCase "c" $ rootPosition 31 3 5 @?= 58
  ]

makeSwaps :: [Word64] -> Bool -> Word64 -> [(Word64, Word64)]
makeSwaps dels rootPresent rootPos =
  let
    convert [x,y] = Just (x,y)
    convert (oddLast : []) =
        if rootPresent
            -- this is swapped and xor'd
            -- relative to the go code
            -- so that the map below stays
            -- simple. this triggers on the
            -- last element if the list has
            -- an odd length. (delRemains)
            then Just (oddLast, rootPos `xor` 1)
            else Nothing
    pairs :: [(Word64, Word64)]
    pairs = catMaybes $ map convert $ chunksOf 2 dels
    f (first, second) = (second `xor` 1, first)
  in
    map f pairs


tests7 :: TestTree
tests7 = testGroup "makeSwaps tests"
  [
      testCase "0" $ makeSwaps [] False 60 @?= []
    , testCase "1" $ makeSwaps [2, 7, 9, 15, 19, 24, 28, 31, 33, 35, 36, 41] False 42 @?= [(6, 2), (14, 9), (25, 19), (30, 28), (34, 33), (40, 36)]
    , testCase "2" $ makeSwaps [72, 79, 81] False 84 @?= [(78, 72)]
    , testCase "3" $ makeSwaps [97, 99, 104] False 106 @?= [(98, 97)]
    , testCase "4" $ makeSwaps [113, 115] False 116 @?= [(114, 113)]
    , testCase "5" $ makeSwaps [121] False 122 @?= []
    , testCase "6" $ makeSwaps [] False 124 @?= []
    , testCase "7" $ makeSwaps [5, 9, 10, 12, 17, 27] True 28 @?= [(8, 5), (13, 10), (26, 17)]
    , testCase "8" $ makeSwaps [32, 35, 36, 41, 45] False 46 @?= [(34, 32), (40, 36)]
    , testCase "9" $ makeSwaps [49, 51] False 54 @?= [(50, 49)]
    , testCase "a" $ makeSwaps [57] False 58 @?= []
    , testCase "b" $ makeSwaps [] False 60 @?= []
    , testCase "c" $ makeSwaps [2, 7, 9, 11] False 12 @?= [(6, 2), (10, 9)]
  ]

makeCollapse :: [Word64] -> Bool -> Int -> Word64 -> Word64 -> Int -> Maybe (Word64, Word64)
makeCollapse dels rootPresent row numLeaves nextNumLeaves forestRows =
  let
    delRemains = length dels .&. 1 /= 0
    rootDest = rootPosition nextNumLeaves row forestRows
  in
    case (delRemains, rootPresent) of
      (False, True) ->
        let rootSrc = rootPosition numLeaves row forestRows
        in Just (rootSrc, rootDest)
      (True, False) ->
        let rootSrc = 1 `xor` (last dels)
        in Just (rootSrc, rootDest)
      _ ->
        Nothing

tests8 :: TestTree
tests8 = testGroup "makeCollapse tests"
  [
      testCase "0" $ makeCollapse [0, 4, 9, 12, 25, 26] True 0 29 13 5 @?= Just (28, 12)
    , testCase "1" $ makeCollapse [37, 41, 43, 45] False 1 29 13 5 @?= Nothing
    , testCase "2" $ makeCollapse [49, 51, 52] False 2 29 13 5 @?= Just (53, 50)
    , testCase "3" $ makeCollapse [57] False 3 29 13 5 @?= Just (56, 56)
    , testCase "4" $ makeCollapse [] False 4 29 13 5 @?= Nothing
    , testCase "5" $ makeCollapse [3, 6, 14, 17, 19, 20] True 0 31 17 5 @?= Just (30, 16)
    , testCase "6" $ makeCollapse [35, 37, 40] True 1 31 17 5 @?= Nothing
    , testCase "7" $ makeCollapse [50, 53] False 2 31 17 5 @?= Nothing
    , testCase "8" $ makeCollapse [] False 3 31 17 5 @?= Nothing
    , testCase "9" $ makeCollapse [0, 3, 4] False 0 8 5 3 @?= Just (5, 4)
    , testCase "a" $ makeCollapse [9, 10] False 1 8 5 3 @?= Nothing
    , testCase "b" $ makeCollapse [13] False 2 8 5 3 @?= Just (12, 12)
  ]

makeSwapNextDels :: [Word64] -> Bool -> Int -> [Word64]
makeSwapNextDels dels rootPresent forestRows =
  let
    convert [x,y] = Just $ parent y forestRows
    convert (oddOne : []) =
      if not rootPresent
        then Just $ parent oddOne forestRows
        else Nothing
  in
    catMaybes $ map convert $ chunksOf 2 dels

tests9 :: TestTree
tests9 = testGroup "makeSwapNextDels tests"
  [
      testCase "0" $ makeSwapNextDels [8, 11, 13, 14, 17, 18, 20] False 5 @?= [37, 39, 41, 42]
    , testCase "1" $ makeSwapNextDels [32, 35, 37, 39, 41, 44] False 5 @?= [49, 51, 54]
    , testCase "2" $ makeSwapNextDels [49, 51, 53] False 5 @?= [57, 58]
    , testCase "3" $ makeSwapNextDels [] False 5 @?= []
    , testCase "4" $ makeSwapNextDels [4, 10, 12, 14, 19, 24, 26, 28] True 5 @?= [37, 39, 44, 46]
    , testCase "5" $ makeSwapNextDels [35, 37, 39, 44] False 5 @?= [50, 54]
    , testCase "6" $ makeSwapNextDels [48, 50] False 5 @?= [57]
    , testCase "7" $ makeSwapNextDels [0, 3, 4] False 3 @?= [9, 10]
    , testCase "8" $ makeSwapNextDels [9, 10] False 3 @?= [13]
    , testCase "9" $ makeSwapNextDels [13] False 3 @?= [14]
    , testCase "a" $ makeSwapNextDels [57] False 5 @?= [60]
    , testCase "b" $ makeSwapNextDels [57] True 5 @?= []
  ]

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) = if x < y
                        then x:(merge xs (y:ys))
                        else y:(merge (x:xs) ys)
merge [] xs = xs
merge xs [] = xs

remTransPre :: [Word64] -> Word64 -> Int -> ([[(Word64, Word64)]], [Maybe (Word64, Word64)])
remTransPre dels numLeaves forestRows =
  let
    nextNumLeaves = numLeaves - (intToWord64 $ length dels)
    producer :: Producer (([(Word64, Word64)], (Maybe (Word64, Word64)))) (State [Word64]) ()
    producer =
      forM_ [0 .. forestRows - 1] $ \row -> do
        readDels <- lift $ State.get
        unless (length readDels == 0) $ do
          let preRootPresent = numLeaves .&. (1 `shiftL` row) /= 0
          let rootPos = rootPosition numLeaves row forestRows
          let (gottenDels, rootPresent) = if preRootPresent && last readDels == rootPos
                                              then (init readDels, False)
                                              else (readDels, preRootPresent)
          let (twinNextDels, newDels) = extractTwins gottenDels forestRows
          let swapNextDels = makeSwapNextDels newDels rootPresent forestRows
          lift $ State.put $ merge twinNextDels swapNextDels
          let collapsed = makeCollapse newDels rootPresent row numLeaves nextNumLeaves forestRows
          let swaps = makeSwaps newDels rootPresent rootPos
          yield $ (swaps, collapsed)
    both = fst $ runState (toListM producer) dels
    padding1 = replicate (forestRows - length both) mzero
    padding2 = replicate (forestRows - length both) mzero
  in
    (map fst both ++ padding1, map snd both ++ padding2)

testsA :: TestTree
testsA = testGroup "remTransPre tests"
  [
      testCase "0" $ remTransPre [1, 2, 7, 11, 12, 13, 14, 15, 16, 17, 20, 22, 23, 24, 25, 27, 28, 37, 40, 41, 42] 43 6 @?= ([[(3, 1), (10, 7), (26, 20), (36, 28)], [(68, 65), (74, 72)], [(103, 101)], [(114, 113)], [], []], [Nothing, Just (83, 74), Just (104, 100), Nothing, Just (120, 120), Nothing])
    , testCase "1" $ remTransPre [] 0 5 @?= (replicate 5 [], [Nothing, Nothing, Nothing, Nothing, Nothing])
    , testCase "2" $ remTransPre [0, 1, 2, 3, 4, 6, 12, 13, 15, 17, 18, 19, 20, 22, 24, 26, 27, 28] 29 5 @?= ([[(7, 4), (16, 15), (23, 20)], [(39, 35)], [(50, 48)], [], []], [Just (25, 10), Just (42, 36), Nothing, Just (56, 56), Nothing])
    , testCase "3" $ remTransPre [] 0 4 @?= (replicate 4 [], replicate 4 Nothing)
    , testCase "4" $ remTransPre [2, 3, 5, 8, 10, 11] 12 4 @?= ([[(9, 5)], [], [], []], [Nothing, Just (16, 18), Just (25, 24), Nothing])
    , testCase "5" $ remTransPre [] 0 5 @?= (replicate 5 [], replicate 5 Nothing)
    , testCase "6" $ remTransPre [0, 1, 3, 4, 6, 8, 9, 11, 12, 13, 14, 15, 18, 19, 22, 25, 26, 27] 28 5 @?= ([[(5, 3), (10, 6), (24, 22)], [(35, 32)], [(53, 49)], [], []], [Nothing, Just (40, 36), Nothing, Just (56, 56), Nothing])
    , testCase "7" $ remTransPre [] 0 5 @?= (replicate 5 [], replicate 5 Nothing)
    , testCase "8" $ remTransPre [1, 3, 5, 8, 9, 10, 12] 18 5 @?= ([[(2, 1), (11, 5)], [(39, 33)], [], [], []], [Just (13, 10), Just (40, 36), Nothing, Just (56, 56), Nothing])
    , testCase "9" $ remTransPre [] 0 3 @?= (replicate 3 [], replicate 3 Nothing)
    , testCase "a" $ remTransPre [0, 2, 3] 5 3 @?= ([[(4, 0)], [], []], [Nothing, Just (8, 8), Nothing])
    , testCase "b" $ remTransPre [] 0 5 @?= (replicate 5 [], replicate 5 Nothing)
    , testCase "c" $ remTransPre [3, 4, 6, 7, 11, 12, 13, 15, 16, 17, 18, 19, 20, 24, 27, 28] 29 5 @?= ([[(5, 3), (14, 11), (25, 20)], [], [(50, 49)], [], []], [Just (26, 12), Nothing, Just (53, 50), Just (56, 56), Nothing])
    , testCase "d" $ remTransPre [] 0 5 @?= (replicate 5 [], replicate 5 Nothing)
    , testCase "e" $ remTransPre [1, 3, 5, 6, 7, 9, 11, 17, 20, 25, 26, 28, 29, 30] 31 5 @?= ([[(2, 1), (8, 5), (16, 11), (24, 20)], [(34, 33), (41, 36)], [(53, 49)], [], []], [Just (27, 16)] ++ replicate 4 Nothing)
    , testCase "f" $ remTransPre [] 0 3 @?= (replicate 3 [], replicate 3 Nothing)
    , testCase "g" $ remTransPre [0, 3, 4] 8 3 @?= ([[(2, 0)], [(11, 9)], []], [Just (5, 4), Nothing, Just (12, 12)])
  ]

remTrans2 :: [Word64] -> Word64 -> Int -> [[(Word64, Word64)]]
remTrans2 dels numLeaves forestRows =
  let
    (swaps, collapses) = remTransPre dels numLeaves forestRows
    newCollapses = swapCollapses swaps collapses
    f (swap, (Just (from, to))) | from /= to = swap ++ [(from, to)]
    f (swap, _) = swap
    newSwaps = map f (zip swaps newCollapses)
  in
    newSwaps

testsB :: TestTree
testsB = testGroup "remTrans2 tests"
  [
      testCase "0" $ remTrans2 [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 17, 19, 20, 21, 22, 23, 26] 29 5 @?= [[(16, 3), (27, 19), (28, 10)], [(41, 38), (44, 36)], [(51, 49)], [], []]
    , testCase "1" $ remTrans2 [0, 2, 3, 4, 5, 11] 12 4 @?= [[(10, 0)], [(19, 17), (20, 18)], [], []]
    , testCase "2" $ remTrans2 [1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 16, 19, 20, 22, 23, 24, 27] 28 5 @?= [[(6, 1), (17, 13), (21, 19), (26, 24)], [(41, 33), (44, 36)], [(51, 49)], [], []]
    , testCase "3" $ remTrans2 [1, 2, 5, 8, 11, 12, 15] 18 5 @?= [[(3, 1), (9, 5), (13, 11), (14, 2)], [(37, 33), (40, 36)], [], [], []]
    , testCase "4" $ remTrans2 [0, 2, 3] 5 3 @?= [[(4, 0)], [], []]
    , testCase "5" $ remTrans2 [0, 1, 5, 7, 8, 9, 10, 12, 14, 16, 17, 20, 21, 25, 26, 27] 29 5 @?= [[(6, 5), (13, 10), (24, 14), (28, 12)], [(34, 32), (39, 36), (43, 40)], [(50, 49), (52, 50)], [], []]
    , testCase "6" $ remTrans2 [2, 3, 4, 5, 9, 11, 13, 18, 19, 20, 21, 22, 23, 25] 31 5 @?= [[(10, 9), (24, 13), (30, 10)], [(35, 33), (40, 37), (46, 44)], [(54, 49)], [], []]
    , testCase "7" $ remTrans2 [3, 6, 7] 8 3 @?= [[], [(10, 9)], []]
  ]

updateDirt :: [Word64] -> [(Word64, Word64)] -> Int -> Int -> [Word64]
updateDirt hashDirt swapRow numLeaves rows =
  let
    loop :: [Word64] -> [(Word64, Word64)] -> Word64 -> [Word64]
    loop [] [] prevHash = []
    loop readDirt readSwap prevHash =
      let (popSwap, hashDest) = makeDestInRow (listToMaybe readSwap) (listToMaybe readDirt) rows
          leavesInt = intToWord64 numLeaves
          skip = (not $ inForest hashDest leavesInt rows)
                  || hashDest == 0
                  || hashDest == prevHash
          newDest = if skip then prevHash else hashDest
          rest = if popSwap
                 then loop readDirt (tail readSwap) newDest
                 else loop (tail readDirt) readSwap newDest
      in
        if skip || hashDest `elem` rest
          then rest
          else sort $ hashDest : rest
    deduped = dedupeSwapDirt (sort hashDirt) (sortOn snd swapRow)
  in
    loop deduped swapRow 0

makeDestInRow :: Maybe (Word64, Word64) -> Maybe Word64 -> Int -> (Bool, Word64)
makeDestInRow _              (Just firstDirt) rows =
  (False, parent firstDirt rows)
makeDestInRow (Just (_, to)) _          rows =
  (True, parent to rows)
makeDestInRow _ _ _ = error "both parameters empty"

inForest :: Word64 -> Word64 -> Int -> Bool
inForest pos numLeaves rows | pos < numLeaves = True
inForest pos numLeaves rows =
  let marker = 1 `shiftL` rows
      mask = (marker `shiftL` 1) - 1
      newPos :: Word64
      newPos = snd $ flip runState pos $ do
                 whileM_ (do gotten<-State.get; return $ gotten .&. marker /= 0) $ do
                   gotten <- State.get
                   let newState = ((gotten `shiftL` 1) .&. mask) .|. 1
                   unless (newState /= gotten) $ error "state did not change"
                   State.put newState
  in
      if pos < numLeaves
          then True
          else if pos >= mask
                   then False
                   else (newPos < numLeaves)




dedupeSwapDirt hashDirt swapRow = [x | x <- hashDirt, not $ x `elem` map snd swapRow]

testsD = testGroup "inForest tests"
  [
      testCase "1" $ inForest 102 33 6 @?= True
    , testCase "2" $ inForest 103 33 6 @?= True
    , testCase "3" $ inForest 96 33 6 @?= True
    , testCase "4" $ inForest 98 33 6 @?= True
    , testCase "19" $ inForest 124 33 6 @?= True
    , testCase "20" $ inForest 126 33 6 @?= False
    , testCase "21" $ inForest 35 29 5 @?= True
    , testCase "31" $ inForest 57 29 5 @?= True
    , testCase "32" $ inForest 59 29 5 @?= False
    , testCase "33" $ inForest 56 29 5 @?= True
    , testCase "37" $ inForest 60 29 5 @?= True
    , testCase "38" $ inForest 62 29 5 @?= False
    , testCase "39" $ inForest 4 4 2 @?= True
    , testCase "40" $ inForest 6 4 2 @?= True
    , testCase "41" $ inForest 32 25 5 @?= True
    , testCase "57" $ inForest 60 25 5 @?= True
    , testCase "58" $ inForest 61 25 5 @?= False
    , testCase "59" $ inForest 62 25 5 @?= False
    , testCase "60" $ inForest 32 25 5 @?= True
    , testCase "75" $ inForest 60 25 5 @?= True
    , testCase "76" $ inForest 62 25 5 @?= False
    , testCase "77" $ inForest 14 8 3 @?= True
    , testCase "78" $ inForest 33 24 5 @?= True
    , testCase "79" $ inForest 37 24 5 @?= True
    , testCase "95" $ inForest 61 24 5 @?= False
    , testCase "96" $ inForest 60 24 5 @?= True
    , testCase "97" $ inForest 62 24 5 @?= False
    , testCase "98" $ inForest 18 11 4 @?= True
    , testCase "99" $ inForest 20 11 4 @?= True
    , testCase "100" $ inForest 25 11 4 @?= True
    , testCase "101" $ inForest 25 11 4 @?= True
    , testCase "102" $ inForest 26 11 4 @?= False
    , testCase "103" $ inForest 28 11 4 @?= True
    , testCase "104" $ inForest 30 11 4 @?= False
    , testCase "105" $ inForest 33 23 5 @?= True
    , testCase "118" $ inForest 57 23 5 @?= True
    , testCase "119" $ inForest 58 23 5 @?= False
    , testCase "120" $ inForest 60 23 5 @?= True
    , testCase "121" $ inForest 60 23 5 @?= True
    , testCase "122" $ inForest 60 23 5 @?= True
    , testCase "123" $ inForest 62 23 5 @?= False
    , testCase "124" $ inForest 17 14 4 @?= True
    , testCase "129" $ inForest 28 14 4 @?= True
    , testCase "130" $ inForest 30 14 4 @?= False
    , testCase "131" $ inForest 65 42 6 @?= True
    , testCase "138" $ inForest 100 42 6 @?= True
    , testCase "139" $ inForest 103 42 6 @?= True
    , testCase "140" $ inForest 101 42 6 @?= True
    , testCase "141" $ inForest 96 42 6 @?= True
    , testCase "142" $ inForest 98 42 6 @?= True
    , testCase "143" $ inForest 100 42 6 @?= True
    , testCase "163" $ inForest 120 42 6 @?= True
    , testCase "164" $ inForest 121 42 6 @?= True
    , testCase "165" $ inForest 122 42 6 @?= False
    , testCase "166" $ inForest 124 42 6 @?= True
    , testCase "171" $ inForest 124 42 6 @?= True
    , testCase "172" $ inForest 126 42 6 @?= False
    , testCase "173" $ inForest 66 35 6 @?= True
    , testCase "194" $ inForest 126 35 6 @?= False
    , testCase "195" $ inForest 33 29 5 @?= True
    , testCase "216" $ inForest 61 29 5 @?= False
    , testCase "217" $ inForest 60 29 5 @?= True
    , testCase "218" $ inForest 60 29 5 @?= True
    , testCase "219" $ inForest 62 29 5 @?= False
    , testCase "223" $ inForest 28 13 4 @?= True
    , testCase "224" $ inForest 30 13 4 @?= False
    , testCase "230" $ inForest 96 38 6 @?= True
    , testCase "231" $ inForest 100 38 6 @?= True
    , testCase "232" $ inForest 103 38 6 @?= True
    , testCase "233" $ inForest 96 38 6 @?= True
    , testCase "234" $ inForest 97 38 6 @?= True
    , testCase "235" $ inForest 98 38 6 @?= True
    , testCase "236" $ inForest 101 38 6 @?= True
    , testCase "237" $ inForest 103 38 6 @?= True
    , testCase "238" $ inForest 112 38 6 @?= True
    , testCase "257" $ inForest 126 38 6 @?= False
    , testCase "265" $ inForest 28 13 4 @?= True
    , testCase "266" $ inForest 30 13 4 @?= False
    , testCase "267" $ inForest 32 29 5 @?= True
    , testCase "283" $ inForest 60 29 5 @?= True
    , testCase "284" $ inForest 61 29 5 @?= False
    , testCase "285" $ inForest 60 29 5 @?= True
    , testCase "286" $ inForest 62 29 5 @?= False
    , testCase "287" $ inForest 32 25 5 @?= True
    , testCase "288" $ inForest 34 25 5 @?= True
    , testCase "289" $ inForest 37 25 5 @?= True
    , testCase "290" $ inForest 40 25 5 @?= True
    , testCase "307" $ inForest 60 25 5 @?= True
    , testCase "308" $ inForest 61 25 5 @?= False
    , testCase "309" $ inForest 62 25 5 @?= False
    , testCase "310" $ inForest 34 29 5 @?= True
    , testCase "311" $ inForest 40 29 5 @?= True
    , testCase "326" $ inForest 60 29 5 @?= True
    , testCase "327" $ inForest 61 29 5 @?= False
    , testCase "328" $ inForest 60 29 5 @?= True
    , testCase "329" $ inForest 61 29 5 @?= False
    , testCase "330" $ inForest 62 29 5 @?= False
    , testCase "331" $ inForest 32 21 5 @?= True
    , testCase "339" $ inForest 60 21 5 @?= True
    , testCase "340" $ inForest 62 21 5 @?= False
    , testCase "341" $ inForest 16 11 4 @?= True
    , testCase "346" $ inForest 24 11 4 @?= True
    , testCase "347" $ inForest 26 11 4 @?= False
    , testCase "348" $ inForest 28 11 4 @?= True
    , testCase "349" $ inForest 28 11 4 @?= True
    , testCase "350" $ inForest 28 11 4 @?= True
    , testCase "351" $ inForest 30 11 4 @?= False
    , testCase "352" $ inForest 16 16 4 @?= True
    , testCase "353" $ inForest 18 16 4 @?= True
    , testCase "378" $ inForest 60 24 5 @?= True
    , testCase "379" $ inForest 60 24 5 @?= True
    , testCase "380" $ inForest 61 24 5 @?= False
    , testCase "381" $ inForest 62 24 5 @?= False
    , testCase "382" $ inForest 69 43 6 @?= True
    , testCase "383" $ inForest 75 43 6 @?= True
    , testCase "410" $ inForest 124 43 6 @?= True
    , testCase "411" $ inForest 126 43 6 @?= False
    , testCase "412" $ inForest 34 29 5 @?= True
    , testCase "432" $ inForest 57 29 5 @?= True
    , testCase "433" $ inForest 60 29 5 @?= True
    , testCase "434" $ inForest 61 29 5 @?= False
    , testCase "435" $ inForest 60 29 5 @?= True
    , testCase "438" $ inForest 62 29 5 @?= False
    , testCase "443" $ inForest 28 12 4 @?= True
    , testCase "444" $ inForest 30 12 4 @?= False
    , testCase "445" $ inForest 32 28 5 @?= True
    , testCase "446" $ inForest 38 28 5 @?= True
    , testCase "464" $ inForest 60 28 5 @?= True
    , testCase "465" $ inForest 61 28 5 @?= False
    , testCase "466" $ inForest 62 28 5 @?= False
    , testCase "467" $ inForest 32 18 5 @?= True
    , testCase "483" $ inForest 60 18 5 @?= True
    , testCase "484" $ inForest 62 18 5 @?= False
    , testCase "485" $ inForest 8 5 3 @?= True
    , testCase "486" $ inForest 12 5 3 @?= True
    , testCase "487" $ inForest 14 5 3 @?= False
    , testCase "488" $ inForest 32 29 5 @?= True
    , testCase "489" $ inForest 37 29 5 @?= True
    , testCase "502" $ inForest 61 29 5 @?= False
    , testCase "503" $ inForest 60 29 5 @?= True
    , testCase "504" $ inForest 60 29 5 @?= True
    , testCase "505" $ inForest 60 29 5 @?= True
    , testCase "506" $ inForest 61 29 5 @?= False
    , testCase "507" $ inForest 62 29 5 @?= False
    , testCase "508" $ inForest 32 31 5 @?= True
    , testCase "524" $ inForest 59 31 5 @?= False
    , testCase "525" $ inForest 56 31 5 @?= True
    , testCase "528" $ inForest 59 31 5 @?= False
    , testCase "529" $ inForest 58 31 5 @?= True
    , testCase "534" $ inForest 61 31 5 @?= False
    , testCase "535" $ inForest 62 31 5 @?= False
    , testCase "looping" $ inForest 1023 431 9 @?= False
  ]

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
    a <- runIO $ eitherDecodeFileStrict' "updateDirt.json"
    let Right (jsonRaw :: [([Word64], [Obj], Int, Int, [Word64])]) = a
    forM_ (take 200 jsonRaw) $ \tupl@(a, b, c, d, e) ->
      let
        tupls = [(objFrom o, objTo o) | o <- b]
      in
        it (show (a, tupls, c, d) ++ " matches reference " ++ show e) $ updateDirt a tupls c d `shouldBe` e

prop_iso :: Hedgehog.Property
prop_iso =
  Hedgehog.property $ do
    let vals1 = Gen.word64 $ Range.constantFrom 1 0 maxBound
    let vals2 = Gen.word64 $ Range.constantFrom 1 0 maxBound
    v <- Hedgehog.forAll vals1
    w <- Hedgehog.forAll vals2
    let x :: Word128 = ((word64ToWord128 v) `shiftL` 64) .|. (word64ToWord128 w)
    x === (decode $ encode x)

sha256 :: Word64 -> CLeaf
sha256 x =
  sha256t [x]

sha256t :: Binary a => [a] -> CLeaf
sha256t x = CLeaf (byteSwapWord128 $ decode f) (byteSwapWord128 $ decode s)
  where
    ctx0   = SHA256.init
    ctx    = foldl SHA256.update ctx0 (map (BS.toStrict . encode) x)
    digest = SHA256.finalize ctx
    (f, s) = BS.splitAt 16 $ BS.fromStrict digest

parentHash :: CLeaf -> CLeaf -> CLeaf
parentHash l r =
    sha256t [byteSwapWord128 $ first  l, byteSwapWord128 $ second l, byteSwapWord128 $ first r, byteSwapWord128 $ second r]

hhashRow :: IForest a => a -> [CULong] -> Maybe HForest
hhashRow forest dirt =
    let
        oldpos = ipositions forest
        olddat :: [CLeaf]
        olddat = idata forest
        hashed :: [CLeaf]
        hashed = flip map dirt $ \x ->
          let
            chld = child x (irows forest)
            chldA = olddat !! chld
            chldB = olddat !! ( chld .|. 1)
            abHash = parentHash chldA chldB
          in
            abHash --trace (show (chld, chldA, chldB, abHash)) abHash
        culongToInt :: CULong -> Int
        culongToInt = fromInteger . toInteger
        intDirt :: [Int]
        intDirt = map culongToInt dirt
        written = snd $ flip runState olddat $ do
            forM_ (zip intDirt hashed) $ \(hp :: Int, result :: CLeaf) -> do
                (gotten :: [CLeaf]) <- State.get
                let new = gotten & ix hp .~ result
                State.put new
    in
        Just $ HForest oldpos written (inumleaves forest) (irows forest)

intToCSize = fromInteger . toInteger
intToWord64 = fromInteger . toInteger
ccharToInt = fromInteger . toInteger
ccharToInteger = fromInteger . toInteger
ccharToWord8 = fromInteger . toInteger
word64ToWord128 = fromInteger . toInteger
word64ToInt = fromInteger . toInteger
word64ToCULong = fromInteger . toInteger
cuLongToWord64 = fromInteger . toInteger
cuLongToInt = fromInteger . toInteger
word8ToInt = fromInteger . toInteger
word8ToCChar = fromInteger . toInteger
intToCULong = fromInteger . toInteger

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

testsE :: TestTree
testsE = testGroup "childMany tests" [
    testCase "1" $ childMany 9 1 3 @?= 2
  , testCase "2" $ childMany 8 1 3 @?= 0
  , testCase "3" $ childMany 20 1 4 @?= 8
  , testCase "4" $ childMany 19 1 4 @?= 6
  , testCase "5" $ childMany 22 1 4 @?= 12
  , testCase "6" $ childMany 21 1 4 @?= 10
  , testCase "7" $ childMany 20 1 4 @?= 8
  , testCase "8" $ childMany 18 1 4 @?= 4
  , testCase "9" $ childMany 20 1 4 @?= 8
  , testCase "10" $ childMany 18 1 4 @?= 4
  , testCase "11" $ childMany 5 2 2 @?= 4
  , testCase "12" $ childMany 7 2 2 @?= 4
 ]

detectRow :: Word64 -> Word8 -> Word8
detectRow position rows =
  let
    initialMarker :: Word64
    initialMarker = 1 `shiftL` (word8ToInt rows)
    (_, h) = snd $ flip runState (initialMarker, 0) $
      whileM_ (do (marker, h) <- State.get
                  return $ position .&. marker /= 0
              ) $ do
        (marker, h) <- State.get
        State.put (marker `shiftR` 1, h+1)
  in
    h

testsF = testGroup "detectRow tests" [
    testCase "0" $ detectRow 248 7 @?= 5
  , testCase "1" $ detectRow 180 7 @?= 1
  ]

hremove :: IForest a => a -> [CULong] -> Maybe HForest
hremove f dels =
  let
    nextNumLeaves = inumleaves f - (intToWord64 $ length dels)
    swapRows = remTrans2 (map cuLongToWord64 dels) (inumleaves f) (ccharToInt $ irows f)
    (mf, _) = snd $ flip runState (itohforest f, []) $
      forM_ (zip [0..(irows f)-1] swapRows) $ \(r, row) -> do
        (gotten, dirt) <- State.get
        let hashDirt = updateDirt dirt row (word64ToInt $ inumleaves gotten) (ccharToInt $ irows gotten)
        forM_ row $ \swap -> do
          (gotten, dirt) <- State.get
          let Just new = hswapNodes gotten (word64ToCULong $ fst swap) (word64ToCULong $ snd swap) r
          State.put (new, dirt)
        (gotten, _) <- State.get
        let Just newf = hhashRow gotten (map word64ToCULong hashDirt)
        State.put (newf, hashDirt)
    toRemove = fromList
        [(miniHash, ()) |
            x <- [0..(length dels)-1],
            let intIndex :: Int = (word64ToInt $ inumleaves mf)-(length dels)+x,
            let cLeaf :: CLeaf = (idata mf) !! intIndex,
            let miniHash = firstTwelveBytes cLeaf
        ]
    cleaned = ipositions mf `difference` toRemove
  in
    Just $ HForest cleaned (idata mf) nextNumLeaves (irows mf)

prop_delete :: Hedgehog.Property
prop_delete =
  Hedgehog.property $ do
    let vals = Gen.set (Range.constantFrom 5 100 100) (Gen.word64 $ Range.constantFrom 0 (2 ^ 31) (2 ^ 32 -1))
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

data T a = N (a, CLeaf) (T a) (T a) | L deriving (Functor, Foldable, Traversable)

type I = T Int

genFun :: IForest a => a -> (Int, Int) -> ((Int, CLeaf), Maybe ((Int, Int), (Int, Int)))
genFun forest (idx, row) =
  let
    dat = idata forest
    node = dat !! idx
    firstChild = child (intToCULong idx) (irows forest)
  in
    if row == 0
      then ((idx, node), Nothing) -- don't descend more once we reached height 0
      else ((idx, node), Just ((firstChild, row - 1), (firstChild .|. 1, row - 1)))

myUnfoldTree :: ((Int, Int) -> ((Int, CLeaf), Maybe ((Int, Int), (Int, Int)))) -> (Int, Int) -> I
myUnfoldTree folder root =
    let
      ((idx, leaf), maybeChildren) = folder root
    in
      case maybeChildren of
        Nothing -> N (idx, leaf) L L
        Just ((idx1, leaf1), (idx2, leaf2)) -> N (idx, leaf) (myUnfoldTree folder (idx1, leaf1)) (myUnfoldTree folder (idx2, leaf2))

myUnfoldForest :: ((Int, Int) -> ((Int, CLeaf), Maybe ((Int, Int), (Int, Int)))) -> [(Int, Int)] -> [I]
myUnfoldForest folder = map (myUnfoldTree folder)

set :: T a -> (T a, T a) -> T a
set (N k _ _) (a, b) = N k a b

chldr :: Lens' I (I, I)
chldr = lens (\(N k a b) -> (a, b)) set

zipTrans topZipper = topZipper & fromWithin chldr & focus %~ swap

testDataTreeZipper = let
    zipped = zipper (Data.Tree.Node "a" [])
    transd :: Zipper (Zipper Top Int (Data.Tree.Tree [Char])) Int [Char] = zipped & fromWithin traverse & focus .~ "J"
    in rezip transd

transTree :: I -> I 
transTree f =
    let zipping :: Top :>> I = zipper f
        transd = zipTrans zipping
    in rezip transd

binTreeToDataTree :: I -> ((Int, CLeaf), [I])
binTreeToDataTree (N (idx, leaf) L L) = ((idx, leaf), [])
binTreeToDataTree (N (idx, leaf) t1@(N (cidx1, _) _ _) t2@(N (cidx2, _) _ _)) = ((idx, leaf), [t1, t2])
binTreeToDataTree (N _ _ _) = error "tree not perfect"
binTreeToDataTree L = error "called on leaf"

myTreeToDataTree :: I -> Data.Tree.Tree (Int, CLeaf)
myTreeToDataTree t =
    Data.Tree.unfoldTree binTreeToDataTree t

main :: IO ()
main = do
    let Just forest = forestWithLeaves testLeaves
    --let Just forest2 = prepareInsertion forest 12
    let Just forest3 = addToForest forest testLeaves2
    --let Just forest4 = deleteFromForest forest3 [0, 2, 4]
    putStrLn $ printTree forest3
    let Just forest6 = swapNodes forest3 8 9 1 -- swapping these two nodes on level 1 (just above leaves) will also swap the leaves
    putStrLn $ printTree forest6
    --let Just forest5 = addToForest forest4 testLeaves3
    let (rootPos, rootRows) = getRootsReverse (inumleaves forest6) (ccharToInt $ irows forest6)
    let f :: [I] = myUnfoldForest (genFun forest6) [(word64ToInt rootPos, rowsInThisTree) | (rootPos, rowsInThisTree) <- zip rootPos rootRows]
    putStrLn $ Data.Tree.drawForest (map (fmap show) $ map myTreeToDataTree f)
    let f2 :: [I] = f & ix 1 %~ transTree
    putStrLn $ Data.Tree.drawForest (map (fmap show) $ map myTreeToDataTree f2)

    let li = [  ("prop_swapNodes", prop_swapNodes)
              , ("prop_iso", prop_iso)
              , ("prop_hashRow", prop_hashRow)
              , ("prop_delete", prop_delete)
             ]
    let group = Hedgehog.Group "example" li
    _ <- Hedgehog.checkSequential group

    tree <- testSpec "hspec tests" testsHspec
    defaultMain $ testGroup "all tests" $
        [
          tests,  tests2, tests3, tests4, tests5, tests6, tests7, tests8,
          tests9, testsA, testsB, testsD, testsE, testsF, tree
        ]
