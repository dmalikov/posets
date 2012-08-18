{-# LANGUAGE UnicodeSyntax #-}
module Data.Poset.Graph
  ( graph, connectWith
  ) where

import Control.Monad (ap, when)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (StateT, execStateT, get, put)
import Data.List (nub)
import Data.Maybe (fromMaybe)

data Graph α = Graph [(α,[α])]
  deriving (Eq, Read, Show)

neighbours ∷ Eq α ⇒ Graph α → α → [α]
neighbours (Graph pairs) e = fromMaybe [] $ lookup e pairs

graph ∷ Eq α ⇒ [(α,α)] → Graph α
graph pairs = Graph $ map (ap (,) adjacencyList) elems
  where adjacencyList e = map snd $ filter ((==e) . fst) pairs
        elems = nub $ map fst pairs

connectWith ∷ Eq α ⇒ Graph α → α → [α]
connectWith γ v = runReader (execStateT allSteps [v]) γ

allSteps ∷ Eq α ⇒ StateT [α] (Reader (Graph α)) ()
allSteps = do
  r ← step
  when r allSteps

step ∷ Eq α ⇒ StateT [α] (Reader (Graph α)) Bool
step = do
  γ ← ask
  visitedVertices ← get
  put $ newVertices γ visitedVertices
  return $ visitedVertices /= newVertices γ visitedVertices
    where
      newVertices γ vs = nub $ (++ vs) $ concatMap (γ `neighbours`) vs
