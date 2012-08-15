{-# LANGUAGE UnicodeSyntax #-}
module Data.Poset.Morphism where

import Control.Monad (replicateM)
import Data.Poset
import Prelude hiding ((>>))

import qualified Data.IntMap as IM

-- | Morph states for «morphism»
-- Morph is a map from Poset to Poset
--
type Morph = IM.IntMap Int

-- | Build Morph from list
--
fromList ∷ [(Int,Int)] → Morph
fromList = IM.fromList

-- | Build Morph from permutation
--
fromPerm ∷ [Int] → Morph
fromPerm = fromList . zip [1..]

-- | Show Morph as a list
--
toList ∷ Morph → [(Int,Int)]
toList = IM.toList

-- | Show Morph as a permutation
--
toPerm ∷ Morph → [Int]
toPerm = map snd . toList

-- | Apply Morph to Poset element
--
infixr 7 >>

(>>) ∷ Morph → Int → Int
m >> a = m IM.! a

-- | Check for isotone property
-- a ρ b ⇒ Morph(a) ρ Morph(b)
--
isotone ∷ Poset → Morph → Bool
isotone p@(Poset es _) m = and
  [ (m >> a) `ρ` (m >> b) | a ← es, b ← es, a `ρ` b ]
    where ρ = binaryRelation p

-- | Check for reducibility
-- Morph(a) ρ a
--
reducible ∷ Poset → Morph → Bool
reducible p@(Poset es _) m = and
  [ (m >> a) `ρ` a | a ← es ]
    where ρ = binaryRelation p

-- | Check for idempotency
-- Morph ∘ Morph ≡ Morph
--
idempotent ∷ Poset → Morph → Bool
idempotent (Poset es _) m = and
  [ (m >> m >> a) == (m >> a) | a ← es ]

-- | Check for fixity
-- b ≡ Morph(b) AND a ρ b ⇒ a ≡ Morph(a)
--
fixed ∷ Poset → Morph → Bool
fixed p@(Poset es _) m = and
  [ a == (m >> a) | b ← es, b == (m >> b), a ← es, a `ρ` b ]
    where ρ = binaryRelation p

-- | Check for:
-- - isotone property;
-- - reducibility;
-- - idempotency;
-- - fixity;
--
nice ∷ Poset → Morph → Bool
nice p m = and
  [ isotone p m
  , reducible p m
  , idempotent p m
  , fixed p m
  ]

-- | Generate all possible Morphs over n-size set
--
generate ∷ Int → [Morph]
generate n = map fromPerm $ replicateM n [1..n]
