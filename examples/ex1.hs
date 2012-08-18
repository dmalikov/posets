{-# LANGUAGE UnicodeSyntax #-}

import Control.Monad (replicateM)
import Data.Poset
import Data.Poset.Morphism

p ∷ Poset Int
p = fromPairsE [1..7] [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]

main ∷ IO ()
main =
  mapM_ (print . toPerm) $ filter (nice p) $ generate (length $ elements p)

-- | Generate all possible Morphs over n-size set
--
generate ∷ Int → [Morph Int]
generate n = map fromPerm $ replicateM n [1..n]

-- | Build Morph from permutation
--
fromPerm ∷ [Int] → Morph Int
fromPerm = Morph . zip [1..]

-- | Show Morph as a permutation
--
toPerm ∷ Morph Int → [Int]
toPerm (Morph m) = map snd m
