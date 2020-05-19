{-# LANGUAGE ForeignFunctionInterface, TypeApplications #-}

import System.IO.Unsafe (unsafePerformIO)
import Forest
import Foreign.Ptr (nullPtr, FunPtr)
import Foreign (Ptr, peek, withArrayLen, peekArray, ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C (CSize(CSize), CULong(CULong), CChar(CChar))
import Foreign.Marshal.Alloc (free)
import Foreign.C.String (peekCString)

import Test.Tasty
import Test.Tasty.HUnit

import Data.Functor.Identity (Identity)
import Data.Word (Word64)
import Data.List (inits)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Either (lefts, rights)
import Data.Bits

import Control.Monad.State.Lazy (runState, modify, get, put, lift, State)
import Pipes.Prelude (toListM)
import Pipes (yield, Producer)
import Control.Monad (forM_, unless, mzero)

import Debug.Trace

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

printTree :: Forest -> IO ()
printTree (Forest forestForeignPtr) = do
    withForeignPtr forestForeignPtr $ \forestPtr -> do
        charPtr <- cForestPrint forestPtr
        peeked_string <- peekCString charPtr
        putStrLn peeked_string
        free charPtr

        peeked_forest <- peek forestPtr
        let numpos =
                fromInteger $ toInteger $ num_leaves peeked_forest
            numleaves =
                fromInteger $ toInteger $ leaves_size peeked_forest
        peeked_posmap <-
            peekArray numpos $ position_map peeked_forest
        peeked_leaves <-
            peekArray numleaves $ leaves peeked_forest
        print (numpos, numleaves)
        print peeked_posmap
        print peeked_leaves
        print $ length peeked_leaves

data Forest = Forest (ForeignPtr CForest)

cToHForest :: (Ptr CForest) -> [CLeaf] -> IO (Maybe Forest)
cToHForest oldForestPtr leaves =
    withArrayLen leaves $ \leavesLen leavesPtr -> do
        let cSize = fromInteger $ toInteger leavesLen
        forestPtr <- cForestAdd oldForestPtr leavesPtr cSize
        checkNull forestPtr

deleteFromForest :: Forest -> [CULong] -> Maybe Forest
deleteFromForest (Forest forestForeignPtr) positions =
    unsafePerformIO $
        withForeignPtr forestForeignPtr $ \oldForestPtr ->
            withArrayLen positions $ \positionsLen positionsPtr -> do
                let cSize = fromInteger $ toInteger positionsLen
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

testLeaves :: [CLeaf]
testLeaves = [CLeaf 0x10 0, CLeaf 0x20 0, CLeaf 0x30 0]

testLeaves2 :: [CLeaf]
testLeaves2 = [CLeaf 0x15 0, CLeaf 0x25 0, CLeaf 0x35 0]

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
        modify (\coll -> swapInRow s coll r)
      val <- get
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
        let rootSrc = 1 `xor` (head $ reverse dels)
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
    nextNumLeaves = numLeaves - (fromInteger $ toInteger $ length dels)
    producer :: Producer (Either [(Word64, Word64)] (Maybe (Word64, Word64))) (State [Word64]) ()
    producer =
      forM_ [0 .. forestRows - 1] $ \row -> do
        readDels <- lift $ get
        unless (length readDels == 0) $ do
          let preRootPresent = numLeaves .&. (1 `shiftL` row) /= 0
          let rootPos = rootPosition numLeaves row forestRows
          let (gottenDels, rootPresent) = if preRootPresent && last readDels == rootPos
                                              then (reverse $ tail $ reverse readDels, False)
                                              else (readDels, preRootPresent)
          let (twinNextDels, newDels) = extractTwins gottenDels forestRows
          let swapNextDels = makeSwapNextDels newDels rootPresent forestRows
          lift $ put $ merge twinNextDels swapNextDels
          let collapsed = makeCollapse newDels rootPresent row numLeaves nextNumLeaves forestRows
          let swaps = makeSwaps newDels rootPresent rootPos
          yield $ Left swaps
          yield $ Right collapsed
    both = fst $ runState (toListM producer) dels
    padding1 = replicate (forestRows - length (lefts both)) mzero
    padding2 = replicate (forestRows - length (lefts both)) mzero
  in
    (lefts both ++ padding1, rights both ++ padding2)

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

-- TODO remTrans2

main :: IO ()
main = do
    defaultMain $ testGroup "all tests" $
        [
          tests, tests2, tests3, tests4, tests5, tests6, tests7, tests8, tests9, testsA
        ]

    putStrLn "HASKELL MAIN STARTS"
    let Just forest = forestWithLeaves testLeaves
    printTree forest

    let Just forest2 = prepareInsertion forest 12
    printTree forest2

    let Just forest3 = addToForest forest2 testLeaves2
    printTree forest3

    let Just forest4 = deleteFromForest forest3 [0, 2, 4]
    printTree forest4



--let Tree forestForeignPtr = forest
--forest2 <-

--alloca $ \forestPtr -> do
    --let f = CForest 0 0 nullPtr nullPtr 0
    --poke forestPtr f
