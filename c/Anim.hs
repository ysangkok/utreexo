{-# LANGUAGE ScopedTypeVariables #-}
import qualified GoImplFunctions (f21, Forest)
import Lib (cbTreeToBTree, toCBTree, ipositions, firstTwelveBytes, parent, irows, ccharToInt, getRootsReverse, inumleaves, goIdxToHaskellPath)
import Forest (CLeaf, cleafToText)

import Foreign.C.Types

import Data.Maybe (fromMaybe, catMaybes)
import Data.Tree (Forest, Tree(Node), rootLabel)
import Data.Tree.Lens (branches)
import Data.Function ((&))
import Data.Monoid (First)
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup (Max(Max), Min(Min))
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.Map (Map, lookup)
import Prelude hiding (lookup)
import Data.Word (Word64)
import qualified Data.Text as T
import Data.WideWord.Word128 (Word128)

import Control.Lens.Operators ((.~), (^?), (^..), (%~), (^@..), (<.>))
import Control.Lens.At (ix)
import Control.Lens (each, _2, folded, Getting, itraversed)
import Control.Arrow ((&&&))
--import Data.Profunctor.Optic.Fold1 (folded1)

import Diagrams.TwoD (scale, translateX)
import Diagrams.TwoD.Types (P2, unp2, mkP2, _x)
import Diagrams.TwoD.Layout.Tree (uniqueXLayout)
import Reanimate (scene, fromToS, tweenVar, reanimate, mkBackground, simpleVar, addStatic, translate, Duration, pauseAtEnd, rotate)
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
toSVGElement :: P2 Double -> String -> SVGT.Tree
toSVGElement p2 levelStr =
    translate x y $ Reanimate.scale 0.06 $ rotate 6 $ mkText (T.pack (" " ++ levelStr)) & SVGT.fontFamily .~ pure ["Ubuntu Mono"]
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
svgT :: Word64 -> CChar -> Forest (CLeaf, P2 Double) -> Tree CLeaf -> P2 Double -> Map Word128 Word64 -> [SVGT.Tree]
svgT leavesCount forestRows xOffsetTrees leavesToAnimate dest positions =
  -- Generated Haskell path and Haskell path should be the same.
  [ toSVGElement newPos (show (goIdx, generatedHaskellPath, haskellPath))
    | (
        haskellPath,
        (leaf :: CLeaf, pos :: P2 Double, children :: Forest (CLeaf, P2 Double))
      ) <- withIndices
    , let newPos = if leaf `elem` leavesToAnimate
                     then pos+dest
                     else pos
    , let goIdx = alwaysIndex forestRows positions leaf children
    , let Just generatedHaskellPath = goIdxToHaskellPath forestRows treeRootPositions goIdx
  ]
  where
    treeRootPositions = reverse $ fst $ getRootsReverse leavesCount (ccharToInt forestRows)
    -- This function puts the children of a node inside the node itself
    xOffsetTreeWithChildren :: Tree (a, b) -> Tree (a, b, Forest (a, b))
    xOffsetTreeWithChildren (Node (x, y) children) = Node (x, y, children) (fmap xOffsetTreeWithChildren children)
    -- We need the children to calculate the Go-style indices, so use the previous function on all tree items
    withItself :: Forest (CLeaf, P2 Double, Forest (CLeaf, P2 Double))
    withItself = fmap xOffsetTreeWithChildren xOffsetTrees
    -- Calculate Haskell-style path down tree
    withIndices :: [((Int, [Int]), (CLeaf, P2 Double, Forest (CLeaf, P2 Double)))]
    withIndices = withItself ^@.. itraversed <.> itraversed
    

mkAnim :: GoImplFunctions.Forest -> Animation
mkAnim f =
  let
    layoutedTrees :: Forest (CLeaf, P2 Double)
    layoutedTrees = reverse $ catMaybes $ map (uniqueXLayout 2 2 . cbTreeToBTree) (toCBTree f)

    -- Widths from left to right
    widths = map oneWidth layoutedTrees

    -- Offsets for every tree such that they do not overlap. Offset every tree by the widths of the trees to the left of it.
    -- This will be one longer than amount of trees, but zip below will just ignore that last element.
    xOffsets :: [Double]
    xOffsets = 0 : scanl1 (+) widths

    -- Move all nodes in each tree according to the X-offset corresponding to the tree
    xOffsetTrees :: Forest (CLeaf, P2 Double)
    xOffsetTrees = zip layoutedTrees xOffsets & each %~ treeAndOffsetToOffsetTree

    fromLens :: Getting (First (Tree a)) (Forest a) (Tree a)
    fromLens = ix 1 . branches . ix 1

    destLens :: Getting (First (Tree a)) (Forest a) (Tree a)
    destLens = ix 0 . branches . ix 0 . branches . ix 1

    fromTree = fromMaybe (error "tree to move not found")            $ xOffsetTrees ^? fromLens 
    destTree = fromMaybe (error "tree getting moved onto not found") $ xOffsetTrees ^? destLens
    (_, fromPos) = rootLabel fromTree
    (_, destPos) = rootLabel destTree

    -- `fraction` is the progress of the nodes to move:
    --    0 means they should be unmoved (still at fromPos)
    --    1 means they should be moved all the way to to destPos
    tweenForest :: Double -> SVGT.Tree
    tweenForest fraction =
      let
        scaled = scale fraction (destPos - fromPos)
        onlyCLeaves = fmap fst fromTree
      in
        mkGroup $ svgT (inumleaves f) (irows f) xOffsetTrees onlyCLeaves scaled (ipositions f)
  in
    scene (
      do v <- simpleVar tweenForest 0.0001
         let duration = 1 :: Duration
         -- Without flip, the tween would go 1->0. But we want 0->1.
         tweenVar v duration (flip fromToS 1)
    )
      & addStatic (mkBackground "white")
      & pauseAtEnd 0.5


main :: IO ()
main = reanimate (mkAnim GoImplFunctions.f21)
