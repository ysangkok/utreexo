{-# LANGUAGE ForeignFunctionInterface #-}

import System.IO.Unsafe (unsafePerformIO)
import Forest
import Foreign.Ptr (nullPtr, FunPtr)
import Foreign (Ptr, peek, withArrayLen, peekArray, ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C (CSize(CSize), CULong(CULong), CChar(CChar))
import Foreign.Marshal.Alloc (free)
import Foreign.C.String (peekCString)

import Test.Tasty
import Test.Tasty.HUnit

import Data.Word (Word64)
import Data.List (inits)

import Data.Bits

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
    mask = 2 `shiftL` forestRows - 1
    innerMask :: Int
    innerMask = forestRows - rise + 1
  in
    mask .&.
      -- in the go version, innerMask is a uint64, so if rise > forestRows + 1,
      -- it will be e.g. uint(-1), which will be a really large number
      -- so in effect, a no-op, which is what we do here
      case (innerMask < 0, rise < 0) of
        (True, False) ->
          position `shiftR` rise -- triggered by 'parentMany extreme'
        (False, False) ->
          position `shiftR` rise .|. mask `shiftL` innerMask
        (True, True) ->
          error "noo"
        (False, True) ->
          0 --mask `shiftL` innerMask

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
tests = testGroup "tests"
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
  , testCase "parentMany extreme" $
     parentMany 18 6 2 @?= 0
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
    pairs = zip <*> tail $ nodes
    matching = [ (x,y) | (x,y) <- pairs, x .|. 1 == y ]
    firsts   = map fst matching
    both     = firsts ++ map snd matching
    parents  = map (flip parent row) firsts
    dels    = [ x | x <- nodes, not $ x `elem` both ]
  in
    (parents, dels)


tests2 :: TestTree
tests2 = testGroup "tests2"
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
tests3 = testGroup "tests3"
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
   , testCase "j" $ swapIfDescendant (100, 97) (41, 18) 6 0 2 @?= 0
   , testCase "k" $ swapIfDescendant (52, 49) (24, 16) 2 0 5 @?= 20
  ]

swapInRow :: (Word64, Word64) -> [Maybe (Word64, Word64)] -> Int -> [Maybe (Word64, Word64)]
swapInRow s collapses r =
    let
      maybeUpdateTo idx (from, to) =
          (from, newTo)
        where
          forestRows = length collapses
          mask = swapIfDescendant s (from, to) r idx forestRows
          newTo = to `xor` mask
      fun (maybeSwap, idx) = fmap (maybeUpdateTo idx) maybeSwap
    in
      fmap fun (zip collapses [0..])

tests4 :: TestTree
tests4 = testGroup "tests4"
  [
    testCase "0" $ swapInRow (100, 97) [Just (41, 18), Just (79, 72), Nothing, Nothing, Just (120, 120), Nothing] 2 @?= [Just (41, 6), Just (79, 66), Nothing, Nothing, Just (120, 120), Nothing]
  , testCase "1" $ swapInRow (66, 65) [Just (41, 4), Nothing, Nothing, Just (115, 114), Just (120, 120), Nothing] 1 @?= [Just (41, 2), Nothing, Nothing, Just (115, 114), Just (120, 120), Nothing]
  , testCase "2" $ swapInRow (68, 66) [Just (42, 8), Nothing, Just (102, 100), Nothing, Just (120, 120), Nothing] 1 @?= [Just (42, 4), Nothing, Just (102, 100), Nothing, Just (120, 120), Nothing]
  , testCase "3" $ swapInRow (35, 33) [Just (26, 6), Just (43, 34), Just (48, 48), Just (57, 56), Nothing] 1 @?= [Just (26, 2), Just (43, 34), Just (48, 48), Just (57, 56), Nothing]
  , testCase "4" $ swapInRow (37, 33) [Just (28, 10), Just (44, 36), Just (51, 50), Just (56, 56), Nothing] 1 @?= [Just (28, 2), Just (44, 36), Just (51, 50), Just (56, 56), Nothing]
  , testCase "5" $ swapInRow (57, 56) [Just (20, 14), Just (46, 38), Just (48, 50), Just (57, 56), Nothing] 3 @?= [Just (20, 6), Just (46, 34), Just (48, 48), Just (57, 56), Nothing]
  , testCase "7" $ swapInRow (37, 35) [Just (22, 10), Just (41, 36), Just (51, 50), Just (56, 56), Nothing] 1 @?= [Just (22, 6), Just (41, 36), Just (51, 50), Just (56, 56), Nothing]
  , testCase "8" $ swapInRow (51, 50) [Just (28, 14), Just (44, 38), Just (51, 50), Just (56, 56), Nothing] 2 @?= [Just (28, 10), Just (44, 36), Just (51, 50), Just (56, 56), Nothing]
  , testCase "9" $ swapInRow (100, 97) [Just (41, 16), Nothing, Nothing, Just (115, 114), Just (120, 120), Nothing] 2 @?= [Just (41, 4), Nothing, Nothing, Just (115, 114), Just (120, 120), Nothing]
  , testCase "a" $ swapInRow (101, 98) [Just (42, 20), Nothing, Just (102, 100), Nothing, Just (120, 120), Nothing] 2 @?= [Just (42, 8), Nothing, Just (102, 100), Nothing, Just (120, 120), Nothing]
  , testCase "b" $ swapInRow (51, 50) [Just (22, 14), Just (41, 38), Just (51, 50), Just (56, 56), Nothing] 2 @?= [Just (22, 10), Just (41, 36), Just (51, 50), Just (56, 56), Nothing]
  , testCase "c" $ swapInRow (100, 98) [Nothing, Just (77, 72), Nothing, Just (115, 114), Just (120, 120), Nothing] 2 @?= [Nothing, Just (77, 68), Nothing, Just (115, 114), Just (120, 120), Nothing]
  , testCase "d" $ swapInRow (114, 113) [Nothing, Just (84, 74), Just (104, 100), Nothing, Just (120, 120), Nothing] 3 @?= [Nothing, Just (84, 70), Just (104, 98), Nothing, Just (120, 120), Nothing]
  , testCase "f" $ swapInRow (115, 114) [Just (41, 24), Nothing, Nothing, Just (115, 114), Just (120, 120), Nothing] 3 @?= [Just (41, 16), Nothing, Nothing, Just (115, 114), Just (120, 120), Nothing]
  , testCase "g" $ swapInRow (115, 114) [Nothing, Just (77, 76), Nothing, Just (115, 114), Just (120, 120), Nothing] 3 @?= [Nothing, Just (77, 72), Nothing, Just (115, 114), Just (120, 120), Nothing]
  , testCase "h" $ swapInRow (71, 69) [Just (34, 14), Just (80, 70), Just (102, 98), Just (112, 112), Nothing, Nothing] 1 @?= [Just (34, 10), Just (80, 70), Just (102, 98), Just (112, 112), Nothing, Nothing]
  , testCase "i" $ swapInRow (101, 97) [Just (43, 22), Just (84, 74), Just (103, 100), Nothing, Just (120, 120), Nothing] 2 @?= [Just (43, 6), Just (84, 66), Just (103, 100), Nothing, Just (120, 120), Nothing]
  , testCase "j" $ swapInRow (4045, 4039) [Nothing, Just (2576, 2494), Just (3335, 3294), Just (3714, 3694), Just (3904, 3894), Just (3998, 3994), Just (4046, 4044), Nothing, Just (4082, 4082), Just (4088, 4088), Nothing] 6
                   @?=
                   [Nothing, Just (2576, 2302), Just (3335, 3198), Just (3714, 3646), Just (3904, 3870), Just (3998, 3982), Just (4046, 4044), Nothing, Just (4082, 4082), Just (4088, 4088), Nothing]
  , testCase "k" $ swapInRow (3983, 3977) [Nothing, Just (2576, 2302), Just (3335, 3198), Just (3714, 3646), Just (3904, 3870), Just (3998, 3982), Just (4046, 4044), Nothing, Just (4082, 4082), Just (4088, 4088), Nothing] 5
                   @?=
                   [Nothing, Just (2576, 2206), Just (3335, 3150), Just (3714, 3622), Just (3904, 3858), Just (3998, 3982), Just (4046, 4044), Nothing, Just (4082, 4082), Just (4088, 4088), Nothing]
  , testCase "l" $ swapInRow (52, 49) [Just (24, 16), Nothing, Nothing, Nothing, Nothing] 2 @?= [Just (24, 4), Nothing, Nothing, Nothing, Nothing]
  , testCase "m" $ swapInRow (51, 50) [Just (22, 14), Just (42, 38), Just (51, 50), Just (56, 56), Nothing] 2 @?= [Just (22, 10), Just (42, 36), Just (51, 50), Just (56, 56), Nothing]
  , testCase "n" $ swapInRow (101, 99) [Nothing, Just (82, 74), Just (103, 100), Nothing, Just (120, 120), Nothing] 2 @?= [Nothing, Just (82, 70), Just (103, 100), Nothing, Just (120, 120), Nothing]
  , testCase "o" $ swapInRow (50, 49) [Just (28, 10), Just (43, 36), Nothing, Just (56, 56), Nothing] 2 @?= [Just (28, 6), Just (43, 34), Nothing, Just (56, 56), Nothing]
  , testCase "p" $ swapInRow (51, 50) [Just (25, 12), Nothing, Just (51, 50), Just (56, 56), Nothing] 2 @?= [Just (25, 8), Nothing, Just (51, 50), Just (56, 56), Nothing]
  , testCase "q" $ swapInRow (58, 57) [Just (30, 16), Nothing, Nothing, Nothing, Nothing] 3 @?= [Just (30, 8), Nothing, Nothing, Nothing, Nothing]
  , testCase "r" $ swapInRow (10, 9) [Just (6, 4), Nothing, Just (12, 12)] 1 @?= [Just (6, 2), Nothing, Just (12, 12)]
  ]

-- TODO check parentMany i golang kald fra fejlende swapInRow tests (og tilføj tests)
-- (det er underligt at den sidste parentMany branch bliver kaldt men ingen effekt har)
-- dernæst swapCollapses
-- dernæst remTrans2

main :: IO ()
main = do
    defaultMain $ testGroup "all tests" [tests, tests2, tests3, tests4]

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
