{-# LANGUAGE ForeignFunctionInterface #-}

module GoImplFunctions where

import Lib (IForest(idata, inumleaves, ipositions, irows, itohforest), toCBTree, CBTree, tree21, word64ToInt, word8ToCChar, intToCSize, HForest(HForest))
import Forest (CLeaf, CForest, num_leaves, leaves, leaves_size, mini, pos, height, position_map)

import Foreign.Ptr (nullPtr, FunPtr)
import Foreign (Ptr, peek, withArrayLen, peekArray, ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.C (CSize(CSize), CULong(CULong), CChar(CChar))
import Foreign.Marshal.Alloc (free)
import Foreign.C.String (peekCString)

import Data.Map (fromList)
import Data.Maybe (fromMaybe)

import System.IO.Unsafe (unsafePerformIO)

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

foreign import ccall "libutreexo.h"
    cForestSwapNodes
    :: Ptr CForest
    -> CULong
    -> CULong
    -> CChar
    -> IO (Ptr CForest)

foreign import ccall "libutreexo.h"
    cForestHashRow
    :: Ptr CForest
    -> Ptr CULong
    -> CSize
    -> IO (Ptr CForest)

foreign import ccall "libutreexo.h &cForestFree"
  cForestFree :: FunPtr (Ptr CForest -> IO ())

printTree :: Forest -> String
printTree (Forest forestForeignPtr) = unsafePerformIO $ do
    withForeignPtr forestForeignPtr $ \forestPtr -> do
        charPtr <- cForestPrint forestPtr
        peeked_string <- peekCString charPtr
        free charPtr
        return $ peeked_string

data Forest = Forest (ForeignPtr CForest)

cToHForest :: (Ptr CForest) -> [CLeaf] -> IO (Maybe Forest)
cToHForest _            [] = return Nothing
cToHForest oldForestPtr leavesList =
    withArrayLen leavesList $ \leavesLen leavesPtr -> do
        let cSize = intToCSize leavesLen
        forestPtr <- cForestAdd oldForestPtr leavesPtr cSize
        checkNull forestPtr

deleteFromForest :: Forest -> [CULong] -> Maybe Forest
deleteFromForest forest [] = Just forest
deleteFromForest (Forest forestForeignPtr) positions =
    unsafePerformIO $
        withForeignPtr forestForeignPtr $ \oldForestPtr ->
            withArrayLen positions $ \positionsLen positionsPtr -> do
                let cSize = intToCSize positionsLen
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
forestWithLeaves leavesCount =
    unsafePerformIO $
        cToHForest nullPtr leavesCount

addToForest :: Forest -> [CLeaf] -> Maybe Forest
addToForest forest [] = Just forest
addToForest (Forest forestForeignPtr) leavesCount =
    unsafePerformIO $
        withForeignPtr forestForeignPtr $ \oldForestPtr ->
            cToHForest oldForestPtr leavesCount

--prepareInsertion :: Forest -> CULong -> Maybe Forest
--prepareInsertion (Forest forestForeignPtr) delta =
--    unsafePerformIO $
--        withForeignPtr forestForeignPtr $ \forestPtr -> do
--            newPtr <- cForestPrepareInsertion forestPtr delta
--            checkNull newPtr

swapNodes :: Forest -> CULong -> CULong -> CChar -> Maybe Forest
swapNodes (Forest forestForeignPtr) from to row =
    unsafePerformIO $
        withForeignPtr forestForeignPtr $ \forestPtr -> do
            newPtr <- cForestSwapNodes forestPtr from to row
            checkNull newPtr

hashRow :: Forest -> [CULong] -> Maybe Forest
hashRow (Forest ffptr) dirt =
    unsafePerformIO $
        withForeignPtr ffptr $ \fptr ->
            withArrayLen dirt $ \len pptr -> do
                let csizelen = intToCSize len
                newPtr <- cForestHashRow fptr pptr csizelen
                checkNull newPtr

rows :: Forest -> CChar
rows (Forest ptr) =
    unsafePerformIO $ withForeignPtr ptr $ \x -> do
        cforest <- peek x
        let h = height cforest
        let converted = word8ToCChar h
        return converted

instance IForest Forest where
    itohforest a = HForest (ipositions a) (idata a) (inumleaves a) (irows a)
    irows = rows
    ipositions (Forest forestForeignPtr) =
        let
          miniposses =
              unsafePerformIO $
                  withForeignPtr forestForeignPtr $ \forestPtr -> do
                      peeked_forest <- peek forestPtr
                      let numpos = word64ToInt $ num_leaves peeked_forest
                      peekArray numpos $ position_map peeked_forest
        in
          fromList [(mini x, pos x) | x <- miniposses] -- map (\(CMiniPos mini pos) -> CMiniPos (mini `shiftR` 32) pos) miniposses
    idata (Forest forestForeignPtr) =
        unsafePerformIO $
            withForeignPtr forestForeignPtr $ \forestPtr -> do
                peeked_forest <- peek forestPtr
                let numleaves = word64ToInt $ leaves_size peeked_forest
                peekArray numleaves $ leaves peeked_forest
    inumleaves (Forest fptr) = unsafePerformIO $ do
      withForeignPtr fptr $ \for -> do
        f <- peek for
        return $ num_leaves f

f21 :: [CBTree CLeaf]
f21 = (toCBTree (fromMaybe (error "error t21") (forestWithLeaves tree21)))
