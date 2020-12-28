{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
module Remtrans where

import Debug.Trace (traceM)
import Data.Monoid
import Data.Ord (Down(Down))
import Control.Monad.State.Lazy
import Prelude hiding (break, last)
import Data.Maybe
import Control.Break
import Lib (getRootsReverse, goIdxToHaskellPath)
import GHC.Word
import Control.Lens hiding (Empty, index)
import qualified Data.Tree as D
import Data.Bits
import Data.List hiding (break, last)
import Data.Tree.Pretty



data Tree a = Node {_val :: a, _left :: (Tree a), _right :: (Tree a)} | Empty
  deriving (Show, Eq, Functor, Foldable, Traversable, Ord)

toDat :: Show a => [Tree a] -> [D.Tree String]
toDat forest =
  fmap go forest
  where
  go :: Show a => Tree a -> D.Tree String
  go Empty = D.Node "empty" [] -- while moving out of a forest, we can set the top node to Empty
  go (Node v Empty Empty) = 
    D.Node (show v) []
  go (Node v Empty b) =
    D.Node (show v) ([D.Node "cut" [], go b])
  go (Node v a Empty) =
    D.Node (show v) ([go a, D.Node "cut" []])
  go (Node v a b) = 
    D.Node (show v) ([go a, go b])

makeLenses ''Tree

hasNoProof :: Foldable t => t (a, Bool) -> Bool
hasNoProof = all (not.snd)


removePerfectSubtreeWithNoProof :: [Tree (a, Bool)] -> Maybe (Tree a, [Tree (a, Bool)])
removePerfectSubtreeWithNoProof forest = do
  idx <- findInForest (\(_,x) -> if hasNoProof $ x ^.. leaves then fmap Down (perfectDepth x) else Nothing) forest
  let foundNoProof = idxToLens idx
  let foundNoProof2 = idxToLens idx
  return $ (fmap fst $ forest ^. foundNoProof, forest & foundNoProof2 .~ Empty)
        

-- call this with the index of each bit set
emptyTree :: Int -> a -> Tree a
emptyTree 0 a = Node a Empty Empty
emptyTree i a = Node a (emptyTree (i - 1) a) (emptyTree (i - 1) a)

-- simply just the bits set, like e.g. in 3 the bits 0 and 1 are set
bitsSet :: FiniteBits a => a -> [Int]
bitsSet i = [x | x <- [0 .. finiteBitSize i - countLeadingZeros i], testBit i x]

emptyForest :: Int -> a -> [Tree a]
emptyForest i a = [emptyTree x a | x <- reverse $ bitsSet i]



pathToTraversal :: Int -> [Int] -> Traversal' [Tree a] (Tree a)
pathToTraversal rootIdx branchIndices =
  ix rootIdx . treeBranchesToTraversal branchIndices
  where
  treeBranchesToTraversal :: [Int] -> Traversal' (Tree a) (Tree a)
  treeBranchesToTraversal (0 : rest) = left  . treeBranchesToTraversal rest
  treeBranchesToTraversal (1 : rest) = right . treeBranchesToTraversal rest
  treeBranchesToTraversal [] = id
  treeBranchesToTraversal _ = error "bad index in tree path"

idxToLens :: (Int, [Int]) -> Lens' [Tree a] (Tree a)
idxToLens (a, b) = unsafeSingular (pathToTraversal a b)


treeHasOnlyNothing :: Tree (Maybe a) -> Bool
treeHasOnlyNothing a = 0 == lengthOf (leaves . filtered (has _Just)) a

-- takes the sub-tree pointed to by findPerfectSubtree
--   and partially filled out newStructure
copyLeaves :: forall a m. (Eq a, MonadFail m) => Tree a -> [Tree (Maybe a)] -> m [Tree (Maybe a)]
copyLeaves perfectSubtree newStructure =
  let
  inserted :: Tree (Maybe a)
  inserted = fmap Just perfectSubtree
  in
  case findInForest (\(_,x) -> if treeHasOnlyNothing x && perfectDepth x == perfectDepth perfectSubtree then perfectDepth x else Nothing) newStructure of 
    Just idx ->
      let
      lensToSubtreeWithEmptyLeaves = idxToLens idx
      in
      return $ newStructure & lensToSubtreeWithEmptyLeaves .~ inserted
    Nothing ->
      fail "could not find destination"

compose :: (Functor t, Foldable t) => t (b -> b) -> b -> b
compose = appEndo . foldl1 (<>) . fmap Endo


leavesWithNothing :: Traversal' [Tree (Maybe a)] (Maybe a)
leavesWithNothing = traversed . leaves . filtered (hasn't _Just)


leaves :: Applicative f' => (b -> f' b) -> Tree b -> f' (Tree b)
leaves g (Node z Empty Empty) = Node <$> (g z) <*> (pure Empty) <*> (pure Empty)
leaves g (Node z l r) = Node z <$> leaves g l <*> leaves g r
leaves _ Empty = pure Empty


prog :: forall m a. (MonadFail m, Show a, Eq a) => [Tree a] -> [(Int, [Int])] -> (String -> m ()) -> m [Tree (Maybe a)]
prog perfectStructure dels traceM =
  let
    oldStructure :: [Tree (a, Bool)]
    oldStructure =
       fmap (fmap (, False)) perfectStructure
         & (compose [idxToLens del .~ Empty | del <- dels])
    delWithSiblings :: [(Int, [Int])]
    delWithSiblings = join $ map tuplToSiblings dels
      where
      tuplToSiblings (root, idxs) = [(root, reverse $ last `xor` 1 : previous) | last : previous <- map reverse $ tail $ inits idxs]
                                 ++ [(root,                          previous) |        previous <-                      inits idxs]
    oldStructureWithHaveProof :: [Tree (a, Bool)]
    oldStructureWithHaveProof = (compose siblingSetters) oldStructure
    siblingSetters :: [[Tree (a, Bool)] -> [Tree (a, Bool)]]
    siblingSetters = [idxToLens i . val . _2 .~ True | i <- delWithSiblings]
    leafCountSum = sum $ map (lengthOf leaves) perfectStructure
    newStructure :: [Tree (Maybe a)]
    newStructure = emptyForest (leafCountSum - length dels) Nothing
    lift2 = lift . lift
    drawForestM :: (Show b, MonadFail m) => [Tree b] -> m ()
    drawForestM removedFrom =
      let
      datatrees = toDat removedFrom
      in
      traceM (drawVerticalForest datatrees)
  in do
  traceM "initial forest"
  drawForestM oldStructureWithHaveProof
  flip evalStateT (oldStructureWithHaveProof, newStructure) $
    loop $ do
      (old :: [Tree (a, Bool)], new :: [Tree (Maybe a)]) <- lift get
      let mlens = removePerfectSubtreeWithNoProof old
      case mlens of
        Just (subtree, removedFrom) -> do
          lift2 $ traceM "removed tree"
          lift2 $ drawForestM [subtree]
          lift2 $ traceM "forest after removal"
          lift2 $ drawForestM removedFrom
          lift2 $ traceM "before insertion"
          lift2 $ drawForestM new
          inserted <- lift2 $ copyLeaves subtree new
          lift2 $ traceM "inserted"
          lift2 $ drawForestM inserted
          lift $ put (removedFrom, inserted)
        Nothing -> do
          lift2 $ traceM "ran out of perfect subtrees with no proof, leaves:"
          let depths = map (fromJust . perfectDepth) perfectStructure
          let leafValues :: [a] = fmap fst $ join $ map (uncurry leavesOnDepth) (zip depths old)
          lift2 $ traceM $ show leafValues
          lift2 $ traceM "final, filled with remaining leaves"
          let final :: [Tree (Maybe a)] = new & partsOf leavesWithNothing .~ (map Just leafValues)
          lift2 $ drawForestM final
          --let noMaybe = fmap (\x -> fmap fromJust x) final
          --lift2 $ traceM (D.drawForest $ fmap (\x -> fmap show (toDat x)) noMaybe)
          break final


leavesOnDepth :: Int -> Tree a -> [a]
leavesOnDepth _ Empty = []
leavesOnDepth 1 (Node v _ _) = [v]
leavesOnDepth i (Node _ a b) = leavesOnDepth (i-1) a ++ leavesOnDepth (i-1) b


perfectDepth :: Tree a -> Maybe Int
perfectDepth Empty = Just 0
perfectDepth (Node _ a b) = do
  d1 <- perfectDepth a
  d2 <- perfectDepth b
  if d1 == d2 then
    Just $ 1 + d1
  else
    Nothing

-- find subtrees that matches predicate.
-- initially [] can be passed for path (it is an accumulator)
-- returns path and found tree
findSubtrees :: Ord o => (([Int], Tree a) -> Maybe o) -> [Int] -> (Tree a) -> [([Int], Tree a)]
findSubtrees ordering path w@(Node _ a b) = let
  ta = findSubtrees ordering (path ++ [0]) a
  tb = findSubtrees ordering (path ++ [1]) b
  in
  (case ordering (path,w) of Just _ -> (:) (path,w); Nothing -> id)
    ta ++ tb
findSubtrees _ _ Empty = []


findInForest :: Ord o => (([Int], Tree a) -> Maybe o) -> [Tree a] -> Maybe (Int, [Int])
findInForest ordering forest =
  -- sort on the o, then discard it
  listToMaybe $ map snd $ sortOn (ordering.fst) $ do
    (idx, tree) <- zip [0..] forest
    -- the o is just for ordering
    o@(path, _) <- findSubtrees ordering [] tree
    return $ (o, (idx, path))

number :: [Tree ()] -> [Tree Int]
number inp = fst $ runState (traverse (traverse fun) inp) 0
  where
    fun _ = do { x <- get; modify (+1); return x}

main :: IO ()
main = do
  --print $ (join $ traverse leavesOnly oldStructureWithHaveProof)
  let
    t :: [Tree Int]
    t = number [emptyTree 2 ()] -- TODO check if 2 is correct for the delPath below
    delPath = intWToIntInt $ fromJust $ goIdxToHaskellPath 2 (fst $ getRootsReverse 4 2) 0 -- (0, [0,0])
      where
      intWToIntInt :: (Int, [Word64]) -> (Int, [Int])
      intWToIntInt (x, y) = (fromIntegral x, map fromIntegral y)
  deleted <- prog t [delPath] traceM
  print deleted
