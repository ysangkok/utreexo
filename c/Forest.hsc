{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DeriveDataTypeable       #-}
module Forest where

import Foreign (Storable(..), poke, sizeOf, peek, alignment, Ptr)

import Data.WideWord.Word128 (Word128)
import Data.Word
import Data.Data (Data)
import qualified Data.Text as T

import Control.Monad (ap)

import Data.WideWord.Word128 (showHexWord128, byteSwapWord128)

#include "libutreexo.h"

data CLeaf = CLeaf {
    first :: Word128
  , second :: Word128
} deriving (Eq, Ord, Data)

instance Show CLeaf where
    show (CLeaf one two) = "CLeaf { " ++ take 4 (showHexWord128 (byteSwapWord128 one)) ++ " " ++ take 4 (showHexWord128 (byteSwapWord128 two)) ++ " }"

cleafToText :: CLeaf -> T.Text
cleafToText = T.pack . take 4 . showHexWord128 . byteSwapWord128 . first

data CMiniPos = CMiniPos {
    mini :: Word128
  , pos :: Word64
} deriving (Eq, Show)

data CForest = CForest {
    num_leaves :: Word64
  , height :: Word8
  , position_map :: Ptr CMiniPos
  , leaves :: Ptr CLeaf
  , leaves_size :: Word64
} deriving Show

instance Storable CMiniPos where
    sizeOf _ = #{size minipos}
    alignment _ = alignment (undefined :: Word128)

    peek p = return CMiniPos
      `ap` (#{peek minipos, mini} p)
      `ap` (#{peek minipos, pos} p)

    poke p foo = do
        #{poke minipos, mini} p $ mini foo
        #{poke minipos, pos} p $ pos foo

instance Storable CLeaf where
    sizeOf _ = #{size leaf}
    alignment _ = alignment (undefined :: Word128)

    poke p foo = do
        #{poke leaf, first} p $ first foo
        #{poke leaf, second} p $ second foo

    peek p = return CLeaf
      `ap` (#{peek leaf, first} p)
      `ap` (#{peek leaf, second} p)

instance Storable CForest where
    sizeOf    _ = #{size forest}
    alignment _ = alignment (undefined :: Word64)

    poke p foo = do
        #{poke forest, num_leaves} p $ num_leaves foo
        #{poke forest, height} p $ num_leaves foo
        #{poke forest, position_map} p $ position_map foo
        #{poke forest, leaves} p $ leaves foo
        #{poke forest, leaves_size} p $ leaves_size foo

    peek p = return CForest
              `ap` (#{peek forest, num_leaves} p)
              `ap` (#{peek forest, height} p)
              `ap` (#{peek forest, position_map} p)
              `ap` (#{peek forest, leaves} p)
              `ap` (#{peek forest, leaves_size} p)
