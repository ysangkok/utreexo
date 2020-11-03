{-# LANGUAGE ScopedTypeVariables #-}
import GoImplFunctions (f21)
import Lib (cbTreeToBTree, CBTree)
import Forest (CLeaf, cleafToText)

import Data.Maybe (fromMaybe, catMaybes)
import Data.Tree (Forest, Tree, rootLabel)
import Data.Tree.Lens (branches)
import Data.Function ((&))
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup (Max(Max), Min(Min))
import Data.List.NonEmpty (NonEmpty, fromList)

import Control.Lens.Operators ((.~), (^?), (^..), (%~)) -- <&>
import Control.Lens.At (ix)
import Control.Lens (each, _2, folded)
import Control.Arrow ((&&&))
--import Data.Profunctor.Optic.Fold1 (folded1)

import Diagrams.TwoD (scale, translateX)
import Diagrams.TwoD.Types (P2, unp2, mkP2, _x)
import Diagrams.TwoD.Layout.Tree (uniqueXLayout)
import Reanimate (scene, fromToS, tweenVar, reanimate, mkBackground, simpleVar, addStatic, translate, Duration, pauseAtEnd)
import qualified Reanimate
import Reanimate.Animation(Animation)
import Reanimate.Svg.Constructors (mkText, mkGroup)
import qualified Graphics.SvgTree.Types as SVGT

-- Formulas derived from trial & error and visual inspection
nodeCoord :: P2 Double -> P2 Double
nodeCoord p2 = mkP2 (x/5 - 7) (-y/2 - 2) where (x,y) = unp2 p2

-- Given a tree and an X-offset, offset all nodes in the tree by the X-offset
treeAndOffsetToOffsetTree :: (Tree (CLeaf, P2 Double), Double) -> Tree (CLeaf, P2 Double)
treeAndOffsetToOffsetTree (tree, offset) = (fmap.fmap) (translateX offset) tree

-- Draw a single node, given its position
toSVGElement :: CLeaf -> P2 Double -> SVGT.Tree
toSVGElement lea p2 =
    translate x y $ Reanimate.scale 0.08 $ mkText (cleafToText lea) & SVGT.fontFamily .~ pure ["Ubuntu Mono"]
  where
    (x,y) = unp2 $ nodeCoord p2

-- The max X coord in the tree minus the min X coord in the tree, plus some padding
oneWidth :: Tree (CLeaf, P2 Double) -> Double
oneWidth t =
  let
    xs :: NonEmpty Double
    xs = fromList $ t ^.. folded . _2 . _x -- bad because fromList is partial
    -- xs = toNonEmptyOf (folded1 . _2 . _x) t -- requires profunctor-optics (broken?)
    (Min minX, Max maxX) = foldMap1 (Min &&& Max) xs
  in
    maxX - minX + 5

-- Given a forest, find the given sub-tree and offset it by the given vector, and draw them all
svgT :: Forest (CLeaf, P2 Double) -> Tree CLeaf -> P2 Double -> [SVGT.Tree]
svgT xOffsetTrees leavesToAnimate dest =
  [ toSVGElement leaf newPos
    | (leaf, pos) <- xOffsetTrees ^.. each . each
    , let newPos = if leaf `elem` leavesToAnimate
                     then pos+dest
                     else pos
  ]

mkAnim :: [CBTree CLeaf] -> Animation
mkAnim f =
  let
    layoutedTrees :: Forest (CLeaf, P2 Double)
    layoutedTrees = catMaybes $ map (uniqueXLayout 2 2 . cbTreeToBTree) f

    -- Widths from left to right
    widths = map oneWidth layoutedTrees

    -- Offsets for every tree such that they do not overlap. Offset every tree by the widths of the trees to the left of it.
    -- This will be one longer than amount of trees, but zip below will just ignore that last element.
    xOffsets :: [Double]
    xOffsets = 0 : scanl1 (+) widths

    -- Move all nodes in each tree according to the X-offset corresponding to the tree
    xOffsetTrees :: Forest (CLeaf, P2 Double)
    xOffsetTrees = zip layoutedTrees xOffsets & each %~ treeAndOffsetToOffsetTree

    fromTree = fromMaybe (error "tree to move not found")            $ xOffsetTrees ^? ix 2 . branches . ix 1
    destTree = fromMaybe (error "tree getting moved onto not found") $ xOffsetTrees ^? ix 3 . branches . ix 0 . branches . ix 1
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
        mkGroup $ svgT xOffsetTrees onlyCLeaves scaled
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
main = reanimate (mkAnim f21)
