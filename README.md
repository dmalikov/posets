## Posets

In mathematics, poset is a set with partial order over it.
Partial order is a such order, that not every pair of elements should be compared.

Binary relation ρ should be:
-  reflexive: `α ρ α` ;
-  antisymmetric: if `α ρ β` and `β ρ α` ⇒ `α ≡ β` ;
-  transitive: if `α ρ β` and `β ρ γ` ⇒ `α ρ γ` .


Poset could be defined by list of elements and list of releations between them:

    > let p = fromPairs [1..7] [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]

    > relation p 7 1
    False

Poset could be defined with satisfying reflexivity and transitivity properies:

    > let p_expanded = fromPairsE [1..7] [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]

    > relation p_expanded 7 1
    True


## Morphisms

Morphisms over a posets defined as a `map` over it's elements described as a list of mappings:

    data Morph α = Morph [(α,α)]

Nice morphism over a poset is a morphism satisfying to isotone property, reducibility, idempotency and fixity:

    > let p = fromPairsE [1..7] [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]

    > let m = Morph [(1,1),(2,2),(3,3),(4,4),(5,5),(6,6),(7,7)]

    > nice p m
    True

## Example

Find all nice morphisms over a given poset:

    [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]

It could be done by `generate` method and bruteforcing all of the possible morphisms like a permutations:

    generate ∷ Int → [Morph Int]
    generate n = map fromPerm $ replicateM n [1..n]

    fromPerm ∷ [Int] → Morph Int
    fromPerm = Morph . zip [1..]

    toPerm ∷ Morph Int → [Int]
    toPerm (Morph m) = map snd m

So, finally:

    > mapM_ (print . toPerm) $ filter (nice p) $ generate (length $ elements p)
    [1,2,3,4,5,6,7]
    [3,3,3,5,5,7,7]
    [5,5,5,5,5,7,7]
    [6,6,7,6,7,6,7]
    [7,7,7,7,7,7,7]

