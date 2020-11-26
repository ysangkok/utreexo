{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified GoImplFunctions (f21, Forest)
import Lib (toCBTree, irows, ccharToInt, inumleaves, Path, pathToLens, delsToHsSwaps, goIdxToHaskellPath, getRootsReverse, CBTree(..), Pos, nodeVal)
import Forest (CLeaf, cleafToText)

import Data.Monoid (appEndo, Endo(Endo))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Function ((&))
import Data.Semigroup.Foldable (foldMap1)
import Data.Semigroup (Max(Max), Min(Min))
import Data.List.NonEmpty (NonEmpty, fromList)
import Prelude hiding (lookup)
import Data.Text (pack, Text)

--import Debug.Trace (traceM)
--import Control.Monad (forM_)
import Control.Monad.State.Lazy (lift, execStateT, get, put)
import Control.Lens.Operators ((.~), (^..), (%~), (^.), (^@..))
import Control.Lens (each, _1, _2, _3, folded, partsOf, traversed, itraversed, (.>), view, Lens')
import Control.Lens.Unsound (lensProduct)
import Control.Arrow ((&&&))
--import Data.Profunctor.Optic.Fold1 (folded1)

import Diagrams.TwoD (scale, translateX)
import Diagrams.TwoD.Types (P2, unp2, mkP2, _x)
import DiagramsTree (uniqueXLayout)
import Reanimate (scene, reanimate, mkBackground, addStatic, translate, play, animate, pause)
import qualified Reanimate
import Reanimate.Animation(Animation)
import Reanimate.Svg.Constructors (mkText, mkGroup, withFillColor)
import qualified Graphics.SvgTree.Types as SVGT

type Forest a = [CBTree a]

-- Formulas derived from trial & error and visual inspection
nodeCoord :: P2 Double -> P2 Double
nodeCoord p2 = mkP2 (x/4 - 7) (-y/2 - 2) where (x,y) = unp2 p2

-- Given a tree and an X-offset, offset all nodes in the tree by the X-offset
treeAndOffsetToOffsetTree :: (CBTree (CLeaf, P2 Double), Double) -> CBTree (CLeaf, P2 Double, Bool)
treeAndOffsetToOffsetTree (tree, offset) = fmap (\(x,y) -> (x,translateX offset y, False)) tree

-- Draw a single node, given its position
toSVGElement :: P2 Double -> Text -> Bool -> SVGT.Tree
toSVGElement p2 levelStr highlight =
    translate x y $ Reanimate.scale 0.06 $ (if highlight then withFillColor "red" else id) $ mkText levelStr & SVGT.fontFamily .~ pure ["Ubuntu Mono"]
  where
    (x,y) = unp2 $ nodeCoord p2

-- The max X coord in the tree minus the min X coord in the tree, plus some padding
oneWidth :: CBTree (CLeaf, P2 Double) -> Double
oneWidth t =
  let
    xs :: NonEmpty Double
    xs = Data.List.NonEmpty.fromList $ t ^.. folded . _2 . _x -- bad because fromList is partial
    -- xs = toNonEmptyOf (folded1 . _2 . _x) t -- requires profunctor-optics (broken?)
    (Min minX, Max maxX) = foldMap1 (Min &&& Max) xs
  in
    maxX - minX + 5

-- Given a forest, find the given sub-tree and offset it by the given vector, and draw them all
svgT :: Forest (CLeaf, P2 Double, Bool) -> CBTree CLeaf -> P2 Double -> [SVGT.Tree]
svgT xOffsetTrees leavesToAnimate dest =
  -- Generated Haskell path and Haskell path should be the same.
  [ toSVGElement newPos labelText highlight
    | (goIdx :: Pos, (leaf :: CLeaf, pos :: P2 Double, highlight)) <- xOffsetTrees ^@.. traversed .> itraversed
    , let newPos = if leaf `elem` leavesToAnimate
                     then pos+dest
                     else pos
    , let dataLabel = cleafToText leaf
    , let posLabel = pack $ show goIdx
    , let labelText = dataLabel <> " " <> posLabel
  ]
    

mkAnim :: GoImplFunctions.Forest -> Animation
mkAnim f =
  let
    layoutedTrees :: Forest (CLeaf, P2 Double)
    layoutedTrees = catMaybes $ map (uniqueXLayout 2 2) (toCBTree f)

    -- Widths from left to right
    widths = map oneWidth layoutedTrees

    -- Offsets for every tree such that they do not overlap. Offset every tree by the widths of the trees to the left of it.
    -- This will be one longer than amount of trees, but zip below will just ignore that last element.
    xOffsets :: [Double]
    xOffsets = 0 : scanl1 (+) widths

    -- Move all nodes in each tree according to the X-offset corresponding to the tree
    xOffsetTrees :: Forest (CLeaf, P2 Double, Bool)
    xOffsetTrees = paintDelsRed $ zip layoutedTrees xOffsets & each %~ treeAndOffsetToOffsetTree

    goIndicesToDelete = [4..9]
    paintDelsRed = compose [pathToLens (fromMaybe (error "goidx err") $ goIdxToHaskellPath (irows f) treeRootPositions x) . nodeVal . _3 .~ True | x <- goIndicesToDelete]
    treeRootPositions = fst $ getRootsReverse (inumleaves f) (ccharToInt (irows f))

  in
    -- runState is outside, such that one single state is used in each traversed element
    scene (do
      play $ pause 0.001
      currentForest <- flip execStateT (xOffsetTrees) $ do
        flip traverse (delsToHsSwaps goIndicesToDelete (inumleaves f) (irows f) ^.. traverse.traverse) $ \(from, to) -> do
          currentForest <- get
          let (newForest, a) = makeScene (from, to) currentForest
          put newForest
          lift $ play a
          --forM_ (zip currentForest newForest) $ \(curTree, newTree) -> do
          --  traceM $ ppRender curTree newTree
          --
      play $ snd $ makeScene ((0, []), (0, [])) currentForest
    )
    & addStatic (mkBackground "white")


-- Just calls all the functions in the traversable on the second argument
compose :: (Functor t, Foldable t) => t (a -> a) -> a -> a
compose = appEndo . foldl1 (<>) . fmap Endo


makeScene :: (Path, Path) -> Forest (CLeaf, P2 Double, Bool) -> (Forest (CLeaf, P2 Double, Bool), Animation)
makeScene (from :: Path, dest :: Path) currentForest =
  let
    fromTree = currentForest ^. pathToLens from
    destTree = currentForest ^. pathToLens dest
    (CBNode _ _ (_, fromPos :: P2 Double, _) _ _) = fromTree
    (CBNode _ _ (_, destPos :: P2 Double, _) _ _) = destTree
    onlyCLeaves :: CBTree CLeaf
    onlyCLeaves = fmap (view _1) fromTree
    -- `fraction` is the progress of the nodes to move:
    --    0 means they should be unmoved (still at fromPos)
    --    1 means they should be moved all the way to to destPos
    tweenForest :: Double -> SVGT.Tree
    tweenForest fraction =
      let
        scaled = scale fraction (destPos - fromPos)
      in
        mkGroup $ svgT currentForest onlyCLeaves scaled
    dataAndHighlightSelector :: Lens' (a, b, c) (a, c)
    dataAndHighlightSelector = (lensProduct _1 _3)
    fromDataHighlight :: [(CLeaf, Bool)]
    fromDataHighlight = fromTree ^.. traversed . dataAndHighlightSelector
    destDataHighlight :: [(CLeaf, Bool)]
    destDataHighlight = destTree ^.. traversed . dataAndHighlightSelector
    newForest =
      currentForest & pathToLens from .~ (fromTree & partsOf (traversed . dataAndHighlightSelector) .~ destDataHighlight)
                    & pathToLens dest .~ (destTree & partsOf (traversed . dataAndHighlightSelector) .~ fromDataHighlight)
  in
    (newForest, animate tweenForest)


main :: IO ()
main = reanimate (mkAnim GoImplFunctions.f21)
