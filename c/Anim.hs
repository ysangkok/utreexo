{-# LANGUAGE ScopedTypeVariables #-}
import GoImplFunctions (f21)
import Lib (cbTreeToBTree, chldr, CBTree, inventiveBTreeToCBTree, dataTreeToBTree)
import Forest (CLeaf(CLeaf), cleafToText)

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Tree
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Function((&))
import Control.Lens (_Just)
import Control.Lens.Operators ((.~), (^?)) -- <&>
import Control.Lens.Tuple
import Control.Lens.At (at, ix)
import Data.List (inits)

import Diagrams.TwoD (scale)
import Diagrams.TwoD.Types (P2, unp2, mkP2)
import Diagrams.TwoD.Layout.Tree (uniqueXLayout)
import Reanimate (scene, fromToS, tweenVar, reanimate, mkBackground, simpleVar, addStatic, translate, rotate)
import qualified Reanimate
import Reanimate.Animation(Animation)
import Reanimate.Svg.Constructors (mkText, mkGroup)
import qualified Graphics.SvgTree.Types as SVGT
import Data.Semigroup (Max(Max), Min(Min))

nodeCoord :: P2 Double -> P2 Double
nodeCoord p2 = mkP2 (x/5 - 7) (-y/2-2) where (x,y) = unp2 p2

mkAnim :: [CBTree CLeaf] -> Animation
mkAnim f =
    let
        layoutedTrees :: [Data.Tree.Tree (CLeaf, P2 Double)]
        layoutedTrees = catMaybes $ map (uniqueXLayout 2 2 . cbTreeToBTree) f
        toSVGElement :: CLeaf -> P2 Double -> SVGT.Tree
        toSVGElement lea p2 =
              translate x y $ Reanimate.scale 0.08 $ mkText (cleafToText lea) & SVGT.fontFamily .~ pure ["Ubuntu Mono"]
            where
              (x,y) = unp2 $ nodeCoord p2
        oneWidth t =
          let
            yMinMax (_lea, p2) =
              let
                (x, _) = unp2 p2
              in
                Just (Min x, Max x)
            Just (Min minX, Max maxX) = foldMap yMinMax t
          in
            maxX - minX + 1
        widths = [x+4 | x <- map oneWidth layoutedTrees]
        treesWithOffsets :: [(Data.Tree.Tree (CLeaf, P2 Double), Double)]
        treesWithOffsets = zip layoutedTrees offsets
        treeAndOffsetToOffsetTree :: (Data.Tree.Tree (CLeaf, P2 Double), Double) -> Data.Tree.Tree (CLeaf, P2 Double)
        treeAndOffsetToOffsetTree (tree, offset) = fmap (\(x,y) -> (x, y + mkP2 offset 0)) tree
        offsetTrees = map treeAndOffsetToOffsetTree treesWithOffsets
        toLeafPosMap = Map.fromList.Data.Tree.flatten
        eachTreeMoved :: [Map.Map CLeaf (P2 Double)]
        eachTreeMoved = map toLeafPosMap offsetTrees
        leafToPos :: Map.Map CLeaf (P2 Double)
        leafToPos = Map.unions eachTreeMoved
        offsets :: [Double]
        offsets = reverse $ tail $ reverse $ map sum $ inits widths
        svgT :: CLeaf -> P2 Double -> [SVGT.Tree]
        svgT toMoveLeaf dest = [svgElem
                  | (leaf, pos) <- Map.toList leafToPos
                  , let newPos = if leaf == toMoveLeaf then pos+dest else pos
                  , let svgElem = toSVGElement leaf newPos]
        tweenForest :: Double -> SVGT.Tree
        tweenForest v = let
            cbTreeReconstruction = map (inventiveBTreeToCBTree . dataTreeToBTree) offsetTrees 
            fromTree = fromMaybe (error "err1") $ cbTreeReconstruction ^? ix 2 . chldr . _2
            destTree = fromMaybe (error "err2") $ cbTreeReconstruction ^? ix 3 . chldr . _1
            (fromLeaf, fromPos) = head $ foldMap pure fromTree
            (_,        destPos) = head $ foldMap pure destTree
            scaled = scale v (destPos - fromPos)
          in
            mkGroup $ svgT fromLeaf scaled
    in
        addStatic (mkBackground "white") $ scene $
          do v <- simpleVar tweenForest 0.0001
             tweenVar v 1 $ \val -> fromToS val 1 -- from 0 to 1

main :: IO ()
main = reanimate (mkAnim f21)
