{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified GoImplFunctions (f21, Forest)
import Lib (cbTreeToBTree, toCBTree, ipositions, firstTwelveBytes, parent, irows, ccharToInt, inumleaves, Path, pathToLens, delsToHsSwaps, goLeavesFromIdx)
import Forest (CLeaf, cleafToText)

import Foreign.C.Types

import Data.Maybe (catMaybes)
import Data.Tree (Forest, Tree(Node), levels)
import Data.Function ((&))
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup (Max(Max), Min(Min))
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Map (Map, lookup, unions, fromList)
import Prelude hiding (lookup)
import Data.Word (Word64)
import Data.Text (pack, Text)
import Data.WideWord.Word128 (Word128)

--import Debug.Trace (traceM)
--import Control.Monad (forM_)
import Control.Monad.State.Lazy (lift, runStateT, get, put)
import Control.Lens.Operators ((.~), (^..), (%~), (^.))
import Control.Lens (each, _2, folded, partsOf, traversed)
import Control.Arrow ((&&&))
--import Data.Profunctor.Optic.Fold1 (folded1)

import Diagrams.TwoD (scale, translateX)
import Diagrams.TwoD.Types (P2, unp2, mkP2, _x)
import Diagrams.TwoD.Layout.Tree (uniqueXLayout)
import Reanimate (scene, reanimate, mkBackground, addStatic, translate, pauseAtEnd, pauseAtBeginning, play, animate, pause)
import qualified Reanimate
import Reanimate.Animation(Animation)
import Reanimate.Svg.Constructors (mkText, mkGroup)
import qualified Graphics.SvgTree.Types as SVGT

-- Formulas derived from trial & error and visual inspection
nodeCoord :: P2 Double -> P2 Double
nodeCoord p2 = mkP2 (x/4 - 7) (-y/2 - 2) where (x,y) = unp2 p2

-- Given a tree and an X-offset, offset all nodes in the tree by the X-offset
treeAndOffsetToOffsetTree :: (Tree (CLeaf, P2 Double), Double) -> Tree (CLeaf, P2 Double)
treeAndOffsetToOffsetTree (tree, offset) = (fmap.fmap) (translateX offset) tree

-- Draw a single node, given its position
toSVGElement :: P2 Double -> Text -> SVGT.Tree
toSVGElement p2 levelStr =
    translate x y $ Reanimate.scale 0.06 $ mkText levelStr & SVGT.fontFamily .~ pure ["Ubuntu Mono"]
  where
    (x,y) = unp2 $ nodeCoord p2

-- Calculate Go-style index based on children (the leaves always have an index in positions)
-- So we just take the parent of the child if no index is available in positions.
alwaysIndex :: CChar -> Map Word128 Word64 -> CLeaf -> Forest (CLeaf, P2 Double) -> Word64
alwaysIndex forestRows positions lea children =
    case goIndex of
      Nothing ->
          parent (alwaysIndex forestRows positions firstLeaf firstLeafChildren) (ccharToInt forestRows)
        where
          -- if there is no position in the map, we assume that there is a child
          Node (firstLeaf, _) firstLeafChildren = head children
      Just x -> x
  where
    goIndex = lookup (firstTwelveBytes lea) positions

-- The max X coord in the tree minus the min X coord in the tree, plus some padding
oneWidth :: Tree (CLeaf, P2 Double) -> Double
oneWidth t =
  let
    xs :: NonEmpty Double
    xs = Data.List.NonEmpty.fromList $ t ^.. folded . _2 . _x -- bad because fromList is partial
    -- xs = toNonEmptyOf (folded1 . _2 . _x) t -- requires profunctor-optics (broken?)
    (Min minX, Max maxX) = foldMap1 (Min &&& Max) xs
  in
    maxX - minX + 5

-- Given a forest, find the given sub-tree and offset it by the given vector, and draw them all
svgT :: CChar -> Forest (CLeaf, P2 Double) -> Tree CLeaf -> P2 Double -> Map Word128 Word64 -> [SVGT.Tree]
svgT forestRows xOffsetTrees leavesToAnimate dest positions =
  -- Generated Haskell path and Haskell path should be the same.
  [ toSVGElement newPos labelText
    | (leaf :: CLeaf, pos :: P2 Double, children) <- withItself ^.. traversed.traversed
    , let newPos = if leaf `elem` leavesToAnimate
                     then pos+dest
                     else pos
    , let goIdx = alwaysIndex forestRows positions leaf children
    , let dataLabel = cleafToText leaf
    , let posLabel = pack $ show goIdx
    , let labelText = dataLabel <> " " <> posLabel
  ]
  where
    -- This function puts the children of a node inside the node itself
    xOffsetTreeWithChildren :: Tree (a, b) -> Tree (a, b, Forest (a, b))
    xOffsetTreeWithChildren (Node (x, y) children) = Node (x, y, children) (fmap xOffsetTreeWithChildren children)
    -- We need the children to calculate the Go-style indices, so use the previous function on all tree items
    withItself :: Forest (CLeaf, P2 Double, Forest (CLeaf, P2 Double))
    withItself = fmap xOffsetTreeWithChildren xOffsetTrees
    

mkAnim :: GoImplFunctions.Forest -> Animation
mkAnim f =
  let
    layoutedTrees :: Forest (CLeaf, P2 Double)
    layoutedTrees = catMaybes $ map (uniqueXLayout 2 2 . cbTreeToBTree) (toCBTree f)

    -- Widths from left to right
    widths = map oneWidth layoutedTrees

    -- Offsets for every tree such that they do not overlap. Offset every tree by the widths of the trees to the left of it.
    -- This will be one longer than amount of trees, but zip below will just ignore that last element.
    xOffsets :: [Double]
    xOffsets = 0 : scanl1 (+) widths

    -- Move all nodes in each tree according to the X-offset corresponding to the tree
    xOffsetTrees :: Forest (CLeaf, P2 Double)
    xOffsetTrees = zip layoutedTrees xOffsets & each %~ treeAndOffsetToOffsetTree

    --fromLens :: Getting (First (Tree a)) (Forest a) (Tree a)
    --fromLens = ix 1 . branches . ix 1

    --destLens :: Getting (First (Tree a)) (Forest a) (Tree a)
    --destLens = ix 0 . branches . ix 0 . branches . ix 1
    goIndicesToDelete = [4..9]

  in
    -- runState is outside, such that one single state is used in each traversed element
    scene (do
      play $ pause 0.001
      flip runStateT (ipositions f, xOffsetTrees) $
        flip traverse (delsToHsSwaps goIndicesToDelete (inumleaves f) (irows f) ^.. traverse.traverse) $ \(fromPos, from, to, toPos) -> do
          (currentPositions, currentForest) <- get
          let (newPositions, newForest, a) = makeScene (inumleaves f) (irows f) (fromPos, from, to, toPos) currentPositions currentForest
          put (newPositions, newForest)
          lift $ play a
          --forM_ (zip currentForest newForest) $ \(curTree, newTree) -> do
          --  traceM $ ppRender curTree newTree
    )
    & addStatic (mkBackground "white")

--do

-- (Positions,Forest) is the state, Scene is the 'inner monad' (that operations in are lifted from, to get to StateT)
makeScene :: Word64 -> CChar -> (Word64, Path, Path, Word64) -> Map Word128 Word64 -> Forest (CLeaf, P2 Double) -> (Map Word128 Word64, Forest (CLeaf, P2 Double), Animation)
makeScene numLeaves numRows (fromGoIdx :: Word64, from :: Path, dest :: Path, destGoIdx :: Word64) positions currentForest =
  let
    fromTree = currentForest ^. pathToLens from
    destTree = currentForest ^. pathToLens dest
    (Node (_, fromPos :: P2 Double) _) = fromTree
    (Node (_, destPos :: P2 Double) _) = destTree
    onlyCLeaves = fmap fst fromTree
    -- `fraction` is the progress of the nodes to move:
    --    0 means they should be unmoved (still at fromPos)
    --    1 means they should be moved all the way to to destPos
    tweenForest :: Double -> SVGT.Tree
    tweenForest fraction =
      let
        scaled = scale fraction (destPos - fromPos)
      in
        mkGroup $ svgT numRows currentForest onlyCLeaves scaled positions
    destLeaves = last $ levels destTree
    fromLeaves = last $ levels fromTree
    destReplacementLeaves = Data.Map.fromList
        [(firstTwelveBytes lea, leafPos)
        | (leafPos, (lea, _)) <- zip (goLeavesFromIdx destGoIdx numRows numLeaves) fromLeaves
        ]
    fromReplacementLeaves = Data.Map.fromList
        [(firstTwelveBytes lea, leafPos)
        | (leafPos, (lea, _)) <- zip (goLeavesFromIdx fromGoIdx numRows numLeaves) destLeaves
        ]
    newPositions = unions [destReplacementLeaves, fromReplacementLeaves, positions]
    fromPositions :: [P2 Double]
    fromPositions = fromTree ^.. traversed . _2
    destPositions :: [P2 Double]
    destPositions = destTree ^.. traversed . _2
    newForest =
      currentForest & pathToLens from .~ (destTree & partsOf (traversed . _2) .~ fromPositions)
                    & pathToLens dest .~ (fromTree & partsOf (traversed . _2) .~ destPositions)
  in
    (newPositions, newForest,
     animate tweenForest -- & pauseAtBeginning 0.5 -- TODO scale by zero, why?
    )


main :: IO ()
main = reanimate (mkAnim GoImplFunctions.f21)
