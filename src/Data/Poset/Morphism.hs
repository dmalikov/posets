{-# LANGUAGE UnicodeSyntax #-}
module Data.Poset.Morphism where

import Data.Poset
import Prelude hiding ((>>))

-- | Morph states for «morphism»
-- Morph is a map from Poset to Poset
--
data Morph α = Morph [(α,α)]


-- | Apply Morph to Poset element
--
infixr 7 >>

(>>) ∷ Eq α ⇒ Morph α → α → α
(Morph m) >> a = head $ map snd $ filter ((== a) . fst) m

-- | Check for isotone property
-- a ρ b ⇒ Morph(a) ρ Morph(b)
--
isotone ∷ Eq α ⇒ Poset α → Morph α → Bool
isotone (Poset es ρ) m = and
  [ (m >> a) `ρ` (m >> b) | a ← es, b ← es, a `ρ` b ]

-- | Check for reducibility
-- Morph(a) ρ a
--
reducible ∷ Eq α ⇒ Poset α → Morph α → Bool
reducible (Poset es ρ) m = and
  [ (m >> a) `ρ` a | a ← es ]

-- | Check for idempotency
-- Morph ∘ Morph ≡ Morph
--
idempotent ∷ Eq α ⇒ Poset α → Morph α → Bool
idempotent (Poset es _) m = and
  [ (m >> m >> a) == (m >> a) | a ← es ]

-- | Check for fixity
-- b ≡ Morph(b) AND a ρ b ⇒ a ≡ Morph(a)
--
fixed ∷ Eq α ⇒ Poset α → Morph α → Bool
fixed (Poset es ρ) m = and
  [ a == (m >> a) | b ← es, b == (m >> b), a ← es, a `ρ` b ]

-- | Check for:
-- - isotone property;
-- - reducibility;
-- - idempotency;
-- - fixity;
--
nice ∷ Eq α ⇒ Poset α → Morph α → Bool
nice p m = and
  [ isotone p m
  , reducible p m
  , idempotent p m
  , fixed p m
  ]
