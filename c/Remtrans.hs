{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
module Remtrans where

import Data.Monoid
import Data.Ord (comparing, Down(Down))
import Control.Monad.State.Lazy
import Prelude hiding (break, last)
import Data.Maybe
import Control.Break
import Lib (getRootsReverse, goIdxToHaskellPath)
import GHC.Word
import Control.Lens hiding (Empty, index, filtered)
import qualified Data.Tree as D
import Data.Bits
import Data.List hiding (break, last)



data Tree a = Node {_val :: a, _left :: (Tree a), _right :: (Tree a)} | Empty
  deriving (Show, Eq, Functor, Foldable, Traversable)


toDat :: (Tree a) -> (D.Tree (Maybe a))
toDat Empty = D.Node Nothing []
toDat (Node v a b) = D.Node (Just v) [toDat a, toDat b]

makeLenses ''Tree

-- could be built from HasSiblings
--  [Node (6,False)
--    (Node (4,False)
--      (Node (0,False) Empty Empty)
--      (Node (1,False) Empty Empty))
--    (Node (5,False)
--      (Node (2,False) Empty Empty)
--      (Node (3,False) Empty Empty))]
    
    



hasNoProof :: Foldable t => t (a, Bool) -> Bool
hasNoProof = all (not.snd)


removePerfectSubtreeWithNoProof :: [Tree (a, Bool)] -> Maybe (Tree a, [Tree (a, Bool)])
removePerfectSubtreeWithNoProof forest = do
  idx <- findInForest (\(_,x) -> if hasNoProof $ leavesOnly x then perfectDepth x else Nothing) forest
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
treeHasOnlyNothing a = length (catMaybes (leavesOnly a)) == 0

-- takes the sub-tree pointed to by findPerfectSubtree
--   and partially filled out newStructure
copyLeaves :: forall a. Eq a => Tree a -> [Tree (Maybe a)] -> [Tree (Maybe a)]
copyLeaves perfectSubtree newStructure =
  let
  inserted :: Tree (Maybe a)
  inserted = fmap Just perfectSubtree
  in
  case findInForest (\(_,x) -> if treeHasOnlyNothing x then perfectDepth x else Nothing) newStructure of 
    Just idx ->
      let
      lensToSubtreeWithEmptyLeaves = idxToLens idx
      in
      newStructure & lensToSubtreeWithEmptyLeaves .~ inserted
    Nothing ->
      error "could not find destination"
  --[ Node (Just 5)
  --    (Node (Just 2) Empty Empty)
  --    (Node (Just 3) Empty Empty)
  --, Node Nothing Empty Empty
  --]

-- take an almost filled tree, fill out the remaining nodes, remove the Maybe
fillRemaining :: [Tree (Maybe a)] -> [a] -> [Tree (Maybe a)]
fillRemaining forest [] = forest
fillRemaining forest (newVal: newVals) =
  let
  index (path, Node Nothing Empty Empty) = Just (Down $ length path)
  index _ = Nothing
  in
  case findInForest index forest of
      Just path ->
        let lensToFreeSlot = idxToLens path
        in fillRemaining (forest & lensToFreeSlot . val .~ Just newVal) newVals
      Nothing -> forest

compose :: (Functor t, Foldable t) => t (b -> b) -> b -> b
compose = appEndo . foldl1 (<>) . fmap Endo

prog :: forall a m. (Monad m, Show a, Eq a) => [Tree a] -> (Int, [Int]) -> (String -> m ()) -> m [Tree a]
prog perfectStructure delPath traceM =
  let
    oldStructure :: [Tree (a, Bool)]
    oldStructure =
       fmap (fmap (, False)) perfectStructure
         & idxToLens delPath .~ Empty
    delWithSiblings :: [(Int, [Int])]
    delWithSiblings = tuplToSiblings delPath
      where
      tuplToSiblings (root, idxs) = [(root, reverse $ last `xor` 1 : previous) | last : previous <- map reverse $ tail $ inits idxs]
                                 ++ [(root,                          previous) |        previous <-                      inits idxs]
    oldStructureWithHaveProof :: [Tree (a, Bool)]
    oldStructureWithHaveProof = (compose siblingSetters) oldStructure
    siblingSetters :: [[Tree (a, Bool)] -> [Tree (a, Bool)]]
    siblingSetters = [idxToLens i . val . _2 .~ True | i <- delWithSiblings]
    Just depth = perfectDepth (perfectStructure !! fst delPath)
    -- always perfect, inferrable from nextNumLeaves (powers of two)
    newStructure :: [Tree (Maybe a)]
    newStructure = emptyForest (2 ^ (depth - 1) - 1) Nothing -- just delete one
    lift2 = lift . lift
  in
  flip evalStateT (oldStructureWithHaveProof, newStructure) $ 
    loop $ do
      (old, new) <- lift get
      let mlens = removePerfectSubtreeWithNoProof old
      case mlens of
        Just (subtree, removedFrom) -> do
          lift2 $ traceM "removed tree"
          lift2 $ traceM (D.drawTree $ fmap show (toDat subtree))
          lift2 $ traceM "forest after removal"
          lift2 $ traceM (D.drawForest $ fmap (\x -> fmap show (toDat x)) removedFrom)
          lift2 $ traceM "before insertion"
          lift2 $ traceM (D.drawForest $ fmap (\x -> fmap show (toDat x)) new)
          let inserted = copyLeaves subtree new
          lift2 $ traceM "inserted"
          lift2 $ traceM (D.drawForest $ fmap (\x -> fmap show (toDat x)) inserted)
          lift $ put (removedFrom, inserted)
        Nothing -> do
          lift2 $ traceM "ran out of perfect subtrees with no proof, leaves:"
          -- t2ree has only nodes with proof
          let leaves :: [a] = fmap fst $ join $ map leavesOnly old
          lift2 $ traceM $ show leaves
          lift2 $ traceM "final, filled with remaining leaves"
          let final :: [Tree (Maybe a)] = fillRemaining new leaves
          lift2 $ traceM (D.drawForest $ fmap (\x -> fmap show (toDat x)) final)
          let noMaybe = fmap (\x -> fmap fromJust x) final
          lift2 $ traceM (D.drawForest $ fmap (\x -> fmap show (toDat x)) noMaybe)
          break (noMaybe)


perfectDepth :: Tree a -> Maybe Int
perfectDepth Empty = Just 0
perfectDepth (Node _ a b) = do
  d1 <- perfectDepth a
  d2 <- perfectDepth b
  if d1 == d2 then
    Just $ 1 + d1
  else
    Nothing

leavesOnly :: Tree a -> [a]
leavesOnly Empty = []
leavesOnly (Node a Empty Empty) = [a]
leavesOnly (Node _ b c) = leavesOnly b ++ leavesOnly c

-- find subtrees that matches predicate.
-- initially [] can be passed for path (it is an accumulator)
-- returns path and found tree
findSubtrees :: Ord o => (([Int], Tree a) -> Maybe o) -> [Int] -> (Tree a) -> [([Int], Tree a)]
findSubtrees ordering path w@(Node _ a b) = let
  ta = findSubtrees ordering (path ++ [0]) a
  tb = findSubtrees ordering (path ++ [1]) b
  in
  (case ordering (path,w) of Just _ -> (:) (path,w); Nothing -> id)
    (case (ta, tb) of
      (x: _, y: _) | comparing ordering x y == GT -> tb ++ ta
      _ -> ta ++ tb)
findSubtrees _ _ Empty = []


findInForest :: Ord o => (([Int], Tree a) -> Maybe o) -> [Tree a] -> Maybe (Int, [Int])
findInForest ordering forest = listToMaybe $ do
  (idx, tree) <- zip [0..] forest
  (path, _) <- findSubtrees ordering [] tree
  return $ (idx, path)

--oldStructureWithHaveProof =
--    [Node (6,True)
--      (Node (4,True)
--        Empty
--        (Node (1,True) Empty Empty))
--      (Node (5,True)
--        (Node (2,False) Empty Empty)
--        (Node (3,False) Empty Empty))]

--  print $
--    newStructure @()
--   ==
--    [ Node Nothing
--        (Node Nothing Empty Empty)
--        (Node Nothing Empty Empty)
--    , Node Nothing Empty Empty
--    ]

number :: Tree () -> Tree Int
number inp = fst $ runState (traverse fun inp) 0
  where
    fun _ = do { x <- get; modify (+1); return x}

main :: IO ()
main = do
  --print $ (join $ traverse leavesOnly oldStructureWithHaveProof)
  let
    t :: Tree Int
    t = number $ emptyTree 2 ()
    delPath = intWToIntInt $ fromJust $ goIdxToHaskellPath 2 (fst $ getRootsReverse 4 2) 0 -- (0, [0,0])
      where
      intWToIntInt :: (Int, [Word64]) -> (Int, [Int])
      intWToIntInt (x, y) = (fromIntegral x, map fromIntegral y)
  deleted <- prog [t] delPath putStrLn
  print deleted
