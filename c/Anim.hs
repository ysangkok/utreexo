{-# LANGUAGE ScopedTypeVariables #-}
import Lib (cbTreeToBTree, I, f21)
import Forest (CLeaf(CLeaf), cleafToText)

import Prelude hiding (lookup)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Tree
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Function((&))
import Control.Lens.Operators ((.~)) -- <&>
import Data.List (inits)

import Diagrams.TwoD (scale)
import Diagrams.TwoD.Types (P2, unp2, mkP2)
import Diagrams.TwoD.Layout.Tree (uniqueXLayout)
import Reanimate (sceneAnimation, fromToS, tweenVar, reanimate, mkBackground, simpleVar, addStatic, translate, rotate)
import qualified Reanimate
import Reanimate.Animation(Animation)
import Reanimate.Svg.Constructors (mkText, mkGroup)
import qualified Graphics.SvgTree.Types as SVGT
import Data.Semigroup (Max(Max), Min(Min))

nodeCoord :: P2 Double -> P2 Double
nodeCoord p2 = mkP2 (x/2 - 5) (-y-2) where (x,y) = unp2 p2

mkAnim :: [I] -> Animation
mkAnim f =
    let
        layoutedTrees :: [Data.Tree.Tree (CLeaf, P2 Double)]
        layoutedTrees = catMaybes $ map (uniqueXLayout 2 2) f
        layoutTreeToSVGTree :: Maybe (CLeaf, P2 Double) -> Data.Tree.Tree (CLeaf, P2 Double) -> (Data.Tree.Tree (CLeaf, P2 Double, SVGT.Tree), Double)
        layoutTreeToSVGTree nodeToMoveAndHowFar t =
           let
              tToSVGElement :: (CLeaf, P2 Double) -> SVGT.Tree
              tToSVGElement (lea, p2) =
                    translate x y $ Reanimate.scale 0.1 $ rotate 10 $ mkText (cleafToText lea <> T.pack (" " ++ show moved)) & SVGT.fontFamily .~ pure ["Ubuntu Mono"]
                  where
                    moved = case nodeToMoveAndHowFar of
                              Just (toMatch, xy) | toMatch == lea -> p2 + xy
                              _ -> p2
                    (x,y) = unp2 $ nodeCoord moved
              yMinMax (_lea, p2) =
                let
                  (x, _) = unp2 p2
                in
                  Just (Min x, Max x)
              Just (Min minX, Max maxX) = foldMap yMinMax t
           in
              (fmap (\(lea, p2) -> (lea, p2, tToSVGElement (lea, p2))) t
              , maxX - minX + 1
              )
        (leafToPosSeparated, widths) = unzip $ map (layoutTreeToSVGTree Nothing) layoutedTrees
        tr (lea, oldPos, _tree) = pure (lea, oldPos)
        treesWithOffsets :: [(Data.Tree.Tree (CLeaf, P2 Double, SVGT.Tree), Double)]
        treesWithOffsets = zip leafToPosSeparated offsets
        treeToMap :: Data.Tree.Tree (CLeaf, P2 Double, SVGT.Tree) -> [(CLeaf, P2 Double)]
        treeToMap = foldMap tr
        eachTreeMoved :: [Map.Map CLeaf (P2 Double)]
        eachTreeMoved = map (Map.fromList.treeToMap.fst) treesWithOffsets
        leafToPos :: Map.Map CLeaf (P2 Double)
        leafToPos = Map.unions eachTreeMoved
        offsets :: [Double]
        offsets = reverse $ tail $ reverse $ map sum $ inits widths
        svgT :: (CLeaf, P2 Double) -> [[SVGT.Tree]]
        svgT toMove = [translated
                  | (xOffsetToUse, dict) <- zip offsets (fst $ unzip $ map (layoutTreeToSVGTree $ Just toMove) layoutedTrees)
                  , let (svgElems :: [(CLeaf, P2 Double, SVGT.Tree)]) = foldMap pure dict
                  , let move (_,_,x) = translate xOffsetToUse 0 x
                  , let translated = fmap move svgElems]
        tweenForest :: Double -> SVGT.Tree
        tweenForest v = let
            fromPos = fromMaybe (error "leaf not found") (Map.lookup (CLeaf 0x10 0) leafToPos)
            destPos = fromMaybe (error "leaf not found") (Map.lookup (CLeaf 0x15 0) leafToPos)
            scaled = scale v (destPos - fromPos)
          in
            mkGroup $ map mkGroup $ svgT ((CLeaf 0x10 0), scaled)
    in
        addStatic (mkBackground "white") $ sceneAnimation $
          do v <- simpleVar tweenForest 0.0001
             tweenVar v 1 $ \val -> fromToS val 1 -- from 0 to 1

main :: IO ()
main = reanimate (mkAnim $ map cbTreeToBTree f21)
