{-# LANGUAGE TemplateHaskell           #-}

module DiagramsTree
       (
       uniqueXLayout
       ) where

import Lib (CBTree(CBEmpty, CBNode))

import           Control.Arrow       (second)
import           Control.Monad.State

import           Data.Maybe
import           Data.Functor.Identity

import           Control.Lens        (makeLenses, (+=), (-=))
import           Diagrams

data Pos = Pos { _level :: Int
               , _horiz :: Int
               }
  deriving (Eq, Show)

makeLenses ''Pos

pos2Point :: Num n => n -> n -> Pos -> P2 n
pos2Point cSep lSep (Pos l h) = p2 (fromIntegral h * cSep, -fromIntegral l * lSep)

-- from diagrams-contrib, function was tweaked to do right before left
uniqueXLayout :: Num n => n -> n -> CBTree a -> Maybe (CBTree (a, P2 n))
uniqueXLayout cSep lSep t = (fmap . fmap . second) (pos2Point cSep lSep)
                $ evalState (uniqueXLayout' t) (Pos 0 0)
  where uniqueXLayout' :: CBTree a -> StateT Pos Identity (Maybe (CBTree (a, Pos)))
        uniqueXLayout' CBEmpty         = return Nothing
        uniqueXLayout' (CBNode h p v l r) = do
          down
          r' <- uniqueXLayout' r
          up
          cartesianPos <- mkNode
          down
          l' <- uniqueXLayout' l
          up
          return $ Just (CBNode h p (v, cartesianPos) (fromMaybe CBEmpty l') (fromMaybe CBEmpty r'))
        mkNode = get <* (horiz += 1)

        down = level += 1
        up   = level -= 1
