{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Lib where

import Data.Data (Data)

import Prelude hiding (drop, lookup)

import Data.WideWord.Word128 (Word128(Word128), byteSwapWord128, word128Hi64, word128Lo64)

import qualified Crypto.Hash.SHA256 as SHA256
import Forest

import Data.Binary (Binary(put,get), encode, decode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Map (fromList, Map, difference, lookup)
import Data.Function((&))
import Control.Lens.Operators ((.~), (%~), (^@..)) -- <&>
import Control.Lens.Plated (rewriteOf, transformOf)
import Data.Data.Lens (uniplate)
                        --     set   upd
import Control.Lens.At (at, ix)
import Control.Lens (_1, _2, backwards)
import Control.Lens.Combinators (Lens', Traversal', lens, traversed, itraversed, indices, unsafeSingular)
import Data.Word (Word64, Word8)
import Data.List (inits, sort, sortOn)
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import Data.Bits ((.|.), (.&.), shiftL, shiftR, xor)
import qualified Data.Tree
import Data.TreeDiff.Tree (treeDiff, EditTree(EditNode), Edit(Swp, Ins, Cpy, Del))
import Text.PrettyPrint (render)
import qualified Text.PrettyPrint as PP

import Control.Lens.Indexed (TraversableWithIndex(itraverse), FunctorWithIndex, FoldableWithIndex)
import Control.Zipper (fromWithin, rezip, zipper, focus, Top, (:>>))
import Data.Tuple (swap)

import Control.Applicative (liftA2)
import Control.Monad.State.Lazy (evalState, execState, modify, lift, State)
import qualified Control.Monad.State.Lazy as State (get, put)
import Pipes.Prelude (toListM)
import Pipes (yield, Producer)
import Control.Monad (forM_, unless, mzero)
import Control.Monad.Loops (whileM_)

import Foreign.C (CSize, CULong, CChar)

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
getRootsReverse leavesCount forestRows =
  let
    rowsCountList = [row | row <- [forestRows, forestRows - 1 .. 0], 1 `shiftL` row .&. leavesCount /= 0]
    positionIncrements = inits $ map (\row -> 1 `shiftL` row) rowsCountList
    positions = map sum positionIncrements
    positionsAndRows = zip positions rowsCountList
    mkRoot (position, row) = parentMany position row forestRows
    roots = map mkRoot positionsAndRows
  in
    (reverse roots, reverse rowsCountList)

class IForest a where
    ipositions :: a -> Map Word128 Word64
    idata :: a -> [CLeaf]
    inumleaves :: a -> Word64
    irows :: a -> CChar
    itohforest :: a -> HForest

toRecord :: (IForest a) => Maybe a -> Maybe (Map Word128 Word64, [CLeaf], Word64)
toRecord = fmap (\a -> (ipositions a, idata a, inumleaves a))

data HForest = HForest {
    hpositions :: Map Word128 Word64
  , hdata :: [CLeaf]
  , hnumleaves :: Word64
  , hrows :: CChar
} deriving (Eq, Show)

instance IForest HForest where
    itohforest = id
    ipositions = hpositions
    idata = hdata
    inumleaves = hnumleaves
    irows = hrows

child :: CULong -> CChar -> Int
child position forestRowsChar =
   word64ToInt $ childMany position 1 forestRowsChar

childMany :: CULong -> CChar -> CChar -> Word64
childMany position dropChar forestRowsChar =
  let
    forestRows = ccharToInt forestRowsChar
    drop = ccharToInt dropChar
    mask = (2 `shiftL` forestRows) - 1
  in
    cuLongToWord64 $ (position `shiftL` drop) .&. mask

instance Data.Binary.Binary Word128 where
  put (Word128 a64 b64) = put a64 <> put b64
  get = do
          (a1 :: Word64) <- get
          (a2 :: Word64) <- get
          return $ Word128 { word128Hi64 = a1, word128Lo64 = a2 }

firstTwelveBytes :: CLeaf -> Word128
firstTwelveBytes lea =
    let twelve :: ByteString = BS.take 12 $ encode $ byteSwapWord128 $ first lea
        padding :: ByteString = BS.replicate 4 0
    in byteSwapWord128 $ decode $ twelve <> padding

swapTwo :: Int -> Int -> [a] -> [a]
swapTwo f s xs = zipWith (\x y ->
    if x == f then xs !! s
    else if x == s then xs !! f
    else y) [0..] xs

hswapNodes :: IForest a => a -> CULong -> CULong -> CChar -> Maybe HForest
hswapNodes goforest from to row =
    let
        posi = ipositions goforest
        leavesData = idata goforest
        a = childMany from row (irows goforest)
        b = childMany to   row (irows goforest)
        tempmap = flip execState posi $ do
            let run = 1 `shiftL` (ccharToInt row)
            forM_ [0..run-1] $ \i -> do
                let lA = leavesData !! (word64ToInt $ a+i)
                let lB = leavesData !! (word64ToInt $ b+i)
                let cA = firstTwelveBytes lA
                let cB = firstTwelveBytes lB
                (gotten :: Map Word128 Word64) <- State.get
                State.put $ gotten & at cB .~ Just (a+i)
                                   & at cA .~ Just (b+i)
        newmap = tempmap
        bottomup = flip execState (leavesData, a, b) $ do
            forM_ [0..row] $ \r -> do
                (before, ia, ib) <- State.get
                let run = 1 `shiftL` (ccharToInt $ row - r)
                    after = flip execState before $
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
                fromInt = cuLongToInt from
                toInt = cuLongToInt to
                row0leaf = swapTwo fromInt toInt leavesData
                -- now data has been swapped, swap positions:
                f2 = cuLongToWord64 from
                t2 = cuLongToWord64 to
                lA = row0leaf !! fromInt
                lB = row0leaf !! toInt
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

tree21 :: [CLeaf]
tree21 = [CLeaf i 0 | i <- [1..15]]

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


swapIfDescendant :: (Word64, Word64) -> (Word64, Word64) -> Int -> Int -> Int -> Word64
swapIfDescendant (a_from, a_to) (_, b_to) ar br forestRows =
  let
      hdiff = ar - br
      bup = parentMany b_to hdiff forestRows
      rootMask = a_from `xor` a_to
  in
      if (bup == a_from) /= (bup == a_to)
          then rootMask `shiftL` hdiff
          else 0

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


swapCollapses :: [[(Word64, Word64)]] -> [Maybe (Word64, Word64)] -> [Maybe (Word64, Word64)]
swapCollapses swaps collapses = flip execState collapses $ do
    forM_ [length collapses - 1, length collapses - 2 .. 1] $ \r -> do
      forM_ (swaps !! r) $ \s ->
        modify (\coll -> swapInRow s coll r) -- this would take forestRows
      val <- State.get
      case val !! r of
        Just rowcol -> modify (\coll -> swapInRow rowcol coll r)
        Nothing -> return ()


rootPosition :: Word64 -> Int -> Int -> Word64
rootPosition leavesCount h forestRows =
  let
    mask = 2 `shiftL` forestRows - 1
    before = leavesCount .&. (mask `shiftL` (h + 1))
    shifted = (before `shiftR` h) .|. (mask `shiftL` (forestRows - (h - 1)))
  in
    shifted .&. mask

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
    convert _ = error "impossible because of argument 2 to chunksOf"
    pairs :: [(Word64, Word64)]
    pairs = catMaybes $ map convert $ chunksOf 2 dels
    f (one, two) = (two `xor` 1, one)
  in
    map f pairs


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

makeSwapNextDels :: [Word64] -> Bool -> Int -> [Word64]
makeSwapNextDels dels rootPresent forestRows =
  let
    convert [_,y] = Just $ parent y forestRows
    convert (oddOne : []) =
      if not rootPresent
        then Just $ parent oddOne forestRows
        else Nothing
    convert _ = error "impossible, chunksOf should limit the parameter to size 2"
  in
    catMaybes $ map convert $ chunksOf 2 dels

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
    both = evalState (toListM producer) dels
    padding1 = replicate (forestRows - length both) mzero
    padding2 = replicate (forestRows - length both) mzero
  in
    (map fst both ++ padding1, map snd both ++ padding2)

remTrans2 :: [Word64] -> Word64 -> Int -> [[(Word64, Word64)]]
remTrans2 dels numLeaves forestRows =
  let
    (swaps, collapses) = remTransPre dels numLeaves forestRows
    newCollapses = swapCollapses swaps collapses
    f (pair, (Just (from, to))) | from /= to = pair ++ [(from, to)]
    f (pair, _) = pair
    newSwaps = map f (zip swaps newCollapses)
  in
    newSwaps

updateDirt :: [Word64] -> [(Word64, Word64)] -> Int -> Int -> [Word64]
updateDirt hashDirt swapRow numLeaves rowsCount =
  let
    loop :: [Word64] -> [(Word64, Word64)] -> Word64 -> [Word64]
    loop [] [] _ = []
    loop readDirt readSwap prevHash =
      let (popSwap, hashDest) = makeDestInRow (listToMaybe readSwap) (listToMaybe readDirt) rowsCount
          leavesInt = intToWord64 numLeaves
          skip = (not $ inForest hashDest leavesInt rowsCount)
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
makeDestInRow _              (Just firstDirt) rowsCount =
  (False, parent firstDirt rowsCount)
makeDestInRow (Just (_, to)) _                rowsCount =
  (True, parent to rowsCount)
makeDestInRow _ _ _ = error "both parameters empty"

inForest :: Word64 -> Word64 -> Int -> Bool
inForest position numLeaves _ | position < numLeaves = True
inForest position numLeaves rowsNumber =
  let marker = 1 `shiftL` rowsNumber
      mask = (marker `shiftL` 1) - 1
      newPos :: Word64
      newPos = flip execState position $ do
                 whileM_ (do gotten<-State.get; return $ gotten .&. marker /= 0) $ do
                   gotten <- State.get
                   let newState = ((gotten `shiftL` 1) .&. mask) .|. 1
                   unless (newState /= gotten) $ error "state did not change"
                   State.put newState
  in
      if position < numLeaves
          then True
          else if position >= mask
                   then False
                   else (newPos < numLeaves)




dedupeSwapDirt :: Eq a1 => [a1] -> [(a2, a1)] -> [a1]
dedupeSwapDirt hashDirt swapRow = [x | x <- hashDirt, not $ x `elem` map snd swapRow]

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
        written = flip execState olddat $ do
            forM_ (zip intDirt hashed) $ \(hp :: Int, result :: CLeaf) -> do
                (gotten :: [CLeaf]) <- State.get
                let new = gotten & ix hp .~ result
                State.put new
    in
        Just $ HForest oldpos written (inumleaves forest) (irows forest)

intToWord8 :: Int -> Word8
intToWord8 = fromInteger . toInteger
intToCSize :: Int -> CSize
intToCSize = fromInteger . toInteger
intToWord64 :: Int -> Word64
intToWord64 = fromInteger . toInteger
ccharToInt :: CChar -> Int
ccharToInt = fromInteger . toInteger
ccharToInteger :: CChar -> Integer
ccharToInteger = fromInteger . toInteger
ccharToWord8 :: CChar -> Word8
ccharToWord8 = fromInteger . toInteger
word64ToWord128 :: Word64 -> Word128
word64ToWord128 = fromInteger . toInteger
word64ToInt :: Word64 -> Int
word64ToInt = fromInteger . toInteger
word64ToCULong :: Word64 -> CULong
word64ToCULong = fromInteger . toInteger
cuLongToWord64 :: CULong -> Word64
cuLongToWord64 = fromInteger . toInteger
cuLongToInt :: CULong -> Int
cuLongToInt = fromInteger . toInteger
word8ToInt :: Word8 -> Int
word8ToInt = fromInteger . toInteger
word8ToCChar :: Word8 -> CChar
word8ToCChar = fromInteger . toInteger
intToCULong :: Int -> CULong
intToCULong = fromInteger . toInteger

detectRow :: Word64 -> Word8 -> Word8
detectRow position rowsCount =
  let
    initialMarker :: Word64
    initialMarker = 1 `shiftL` (word8ToInt rowsCount)
    (_, h) = flip execState (initialMarker, 0) $
      whileM_ (do (marker, _) <- State.get
                  return $ position .&. marker /= 0
              ) $ do
        (marker, h2) <- State.get
        State.put (marker `shiftR` 1, h2+1)
  in
    h

hremove :: IForest a => a -> [CULong] -> Maybe HForest
hremove f dels =
  let
    nextNumLeaves = inumleaves f - (intToWord64 $ length dels)
    swapRows = remTrans2 (map cuLongToWord64 dels) (inumleaves f) (ccharToInt $ irows f)
    (mf, _) = flip execState (itohforest f, []) $
      forM_ (zip [0..(irows f)-1] swapRows) $ \(r, row) -> do
        (gotten, dirt) <- State.get
        let hashDirt = updateDirt dirt row (word64ToInt $ inumleaves gotten) (ccharToInt $ irows gotten)
        forM_ row $ \pair -> do
          (gotten2, dirt2) <- State.get
          let Just new = hswapNodes gotten2 (word64ToCULong $ fst pair) (word64ToCULong $ snd pair) r
          State.put (new, dirt2)
        (gotten2, _) <- State.get
        let Just newf = hhashRow gotten2 (map word64ToCULong hashDirt)
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



-- create GADT tree in the same way Data.Tree.unfoldTree works
myUnfoldTree :: IForest a => a -> (Pos, Int) -> CBTree CLeaf
myUnfoldTree a root =
    let
      -- node creation helper function for unfolders in the style of Data.Tree.unfoldTree
      folder :: (Pos, Int) -> (Pos, CLeaf, Maybe ((Pos, Int), (Pos, Int)))
      folder (position@(Pos idx), row) =
        let
          dat = idata a
          node = dat !! idx
          firstChild = child (intToCULong idx) (irows a)
        in
          if row == 0
            then (position, node, Nothing) -- don't descend more once we reached height 0
            else (position, node, Just ((Pos $ firstChild, row - 1), ((Pos $ firstChild .|. 1), row - 1)))
      (p, lea, maybeChildren) = folder root
    in
      case maybeChildren of
        Nothing -> (CBNode (Height $ ccharToInt $ irows a) p lea CBEmpty CBEmpty)
        Just (leaf1, leaf2) -> CBNode (Height $ ccharToInt $ irows a) p lea (myUnfoldTree a leaf1) (myUnfoldTree a leaf2)

-- create GADT forest in the same way Data.Tree.unfoldForest works
myUnfoldForest :: IForest a => a -> [(Pos, Int)] -> [CBTree CLeaf]
myUnfoldForest a = map (myUnfoldTree a)

nodeVal :: Lens' (CBTree a) a
nodeVal = lens cget cset
    where
        cget (CBNode _ _ v _ _) = v
        cget _ = error "only works on non-leaves"
        cset :: (CBTree a) -> a -> (CBTree a)
        cset (CBNode h p _ a b) v = CBNode h p v a b
        cset _ _ = error "only works on non-leaves"

-- children of a GADT tree node
chldr :: Lens' (CBTree a) ((CBTree a), (CBTree a))
chldr = lens cget cset
    where
        cget (CBNode _ _ _ a b) = (a, b)
        cget _ = error "only works on non-leaves"
        cset :: (CBTree a) -> (CBTree a, CBTree a) -> (CBTree a)
        cset (CBNode h p v _ _) (a, b) = CBNode h p v a b
        cset _ _ = error "only works on non-leaves"

printCB :: Show a => CBTree a ->  IO ()
printCB = putStrLn . Data.Tree.drawTree . (fmap show) . cbTreeToDataTree

-- swap children of root node using zipper
--transTree :: I -> I
transTree :: CBTree CLeaf -> CBTree CLeaf
transTree f =
    let zipping :: Top :>> CBTree CLeaf = zipper f
        zipTrans topZipper = topZipper & fromWithin chldr & focus %~ swap
        transd = zipTrans zipping
    in recalcPositions $ head $ sortLeaves [rezip transd]

isLeaf :: Height -> Pos -> Bool
isLeaf (Height h) (Pos p) = detectRow (intToWord64 p) (intToWord8 h) == 0

-- Sort leaf positions, don't touch values
sortLeaves :: forall a. (Data a, Ord a, Show a, Eq a) => [CBTree a] -> [CBTree a]
sortLeaves f@((CBNode h _ _ _ _) : _) =
    -- With partsOf you can only replace values. But we want to replace keys
    -- f & partsOf (traverse.filtered (`elem` (map snd toSort)))
    -- .~ sorted
    map (transformOf uniplate (\org ->
      case org of
        CBNode leafHeight p v a b ->
          case lookup p mapped of
            Just newPos -> CBNode leafHeight newPos v a b
            Nothing -> org
        CBEmpty -> org
    )) f
  where
    toSort :: [Pos]
    toSort = map fst $ f ^@.. backwards traversed . itraversed . indices (isLeaf h)
    sorted :: [Pos]
    sorted = sort toSort
    mapped :: Map Pos Pos
    mapped = fromList $ zip toSort sorted
sortLeaves _ = error "must have at least one tree in forest"

recalcPos :: Height -> Int -> Int -> Pos
recalcPos (Height h) childA childB =
  if parent (intToWord64 childA) h /= parent (intToWord64 childB) h 
    then error $ "children do not have same parent: " ++ show (childA, childB)
    else Pos (word64ToInt $ parent (intToWord64 childA) h)

recalcPosInNode :: Data a => (CBTree a) -> Maybe (CBTree a)
recalcPosInNode (CBNode hei posi val childA@(CBNode _ (Pos posChildA) _ _ _) childB@(CBNode _ (Pos posChildB) _ _ _)) =
  let
    newPos = recalcPos hei posChildA posChildB
  in
    if newPos == posi
      then Nothing
      else Just (CBNode hei newPos val childA childB)
recalcPosInNode _ = Nothing

recalcPositions :: Data a => CBTree a -> CBTree a
recalcPositions = rewriteOf uniplate recalcPosInNode

toCBTree :: IForest a => a -> [CBTree CLeaf]
toCBTree f =
    let
        (rootPositions, rootRows) = getRootsReverse (inumleaves f) (ccharToInt $ irows f)
        roots = [(Pos $ word64ToInt rootPos, rowsInThisTree) | (rootPos, rowsInThisTree) <- zip rootPositions rootRows]
    in
        myUnfoldForest f roots

newtype Height = Height Int
  deriving (Show, Eq, Data)
newtype Pos = Pos Int
  deriving (Show, Eq, Data, Ord)

data CBTree a = CBNode Height Pos a (CBTree a) (CBTree a) | CBEmpty
  deriving (Functor, Foldable, Traversable, Data)


deriving instance Eq a => Eq (CBTree a)
deriving instance FunctorWithIndex Pos CBTree
deriving instance FoldableWithIndex Pos CBTree
instance TraversableWithIndex Pos CBTree where
  itraverse :: forall f a b . Applicative f => (Pos -> a -> f b) -> CBTree a -> f (CBTree b)
  itraverse fun (CBNode h p a left right) = let
      l :: f (CBTree b)
      l = itraverse fun left
      r :: f (CBTree b)
      r = itraverse fun right
      n hei position el (lef, rig) = CBNode hei position el lef rig
      in
      liftA2 (n h p) (fun p a) (liftA2 (,) l r)
  itraverse _ CBEmpty = pure CBEmpty

cbNodeToDataTree :: CBTree a -> ((Height, Pos, a), [CBTree a])
cbNodeToDataTree (CBNode h p cleaf CBEmpty CBEmpty) = ((h, p, cleaf), [])
cbNodeToDataTree (CBNode h p cleaf t1@(CBNode _ _ _ _ _) t2@(CBNode _ _ _ _ _)) = ((h, p, cleaf), [t1, t2])
cbNodeToDataTree (CBNode _ _ _ _ _) = error "tree not perfect"
cbNodeToDataTree CBEmpty = error "called on leaf"

cbTreeToDataTree :: CBTree a -> Data.Tree.Tree (Height, Pos, a)
cbTreeToDataTree t =
    Data.Tree.unfoldTree cbNodeToDataTree t

type Path = (Int, [Word64])

-- |The first tuple element if the index of the tree in the forest
-- |The second tuple element is a list of: 0 for left, 1 for right, descending down from the root
-- |This is the same kinda path you get if you used
-- |(t :: Data.Tree.Forest) ^@.. itraversed <.> itraversed
goIdxToHaskellPath :: CChar -> [Word64] -> Word64 -> Maybe Path
goIdxToHaskellPath forestRows rootPos goIdx = do
    upPath <- goDown goIdx 0
    treeIdx <- lookup (head upPath) rootPosMap
    return (treeIdx, tail upPath)
  where
    rootPosMap = fromList (zip rootPos [0..])
    goDown :: Word64 -> CChar -> Maybe [Word64]
    -- We put the root at the top, so we can look it up above with head
    goDown idx _ | Just _ <- lookup idx rootPosMap = Just [idx]
    goDown _   h | h == forestRows = Nothing -- idx was out of range since we have used our whole budget of rows but did not find a root yet
    goDown idx h = do
      untilHere <- goDown (parent idx (ccharToInt forestRows)) (h+1)
      return $ untilHere ++ [idx .&. 1]

delsToHsSwaps :: [Word64] -> Word64 -> CChar -> [[(Path, Path)]]
delsToHsSwaps dels numLeaves forestRows =
    (fmap.fmap) (\(x,y) -> (toP x, toP y)) (remTrans2 dels numLeaves (ccharToInt forestRows))
  where
    toP x = fromMaybe (error "goToHsPath failed!") $ goIdxToHaskellPath forestRows (fst $ getRootsReverse numLeaves (ccharToInt forestRows)) x


pathToLens :: Path -> Lens' ([CBTree a]) (CBTree a)
pathToLens (rootIdx, bitsPath) =
    unsafeSingular $ ix rootIdx . helper bitsPath
  where
    helper :: [Word64] -> Traversal' (CBTree a) (CBTree a)
    helper (0 : rest) = chldr . _1 . helper rest
    helper (1 : rest) = chldr . _2 . helper rest
    helper [] = id
    helper _ = error "path did contain something that wasn't a bit"


---- Depth-first or breath-first does not matter since only leaves are returned
--goLeavesFromIdx :: Word64 -> CChar -> Word64 -> [Word64]
--goLeavesFromIdx goIdx numRows numLeaves | not $ inForest goIdx numLeaves (ccharToInt numRows) = error "bad idx for goLeavesFromIdx"
--goLeavesFromIdx goIdx numRows numLeaves | detectRow goIdx (ccharToWord8 numRows) == 0 = [goIdx]
--goLeavesFromIdx goIdx numRows numLeaves = [
--  y
--  | x <- [child goIntIdx numRows, child goIntIdx numRows .|. 1]
--  , y <- goLeavesFromIdx (intToWord64 x) numRows numLeaves
--  ]
--  where
--    goIntIdx :: CULong = word64ToCULong goIdx

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

ppRender :: (Eq a, Show a) => Data.Tree.Tree a -> Data.Tree.Tree a -> String
ppRender a b = render $ (ppEditTree (PP.text . show)) (treeDiff a b)
