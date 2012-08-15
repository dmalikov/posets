## Posets

In mathematics, poset is a set with partial order over it.
Partial order is a such order, that not every pair of elements should be related.
Partial order  should be reflexive, antisymmetric, and transitive.

## Int-posets

Int-poset is a poset of `Int`s. It is a nice example and playing ground for modelling morphisms over a posets.

    data Poset = Poset 
      { elements ∷ [Int]
      , relations ∷ [(Int,Int)]
      } deriving (Eq, Show, Read)

Int-poset could be defined by list of elements and list of releations between them:

    > Poset [1..7] [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]
    Poset {elements = [1,2,3,4,5,6,7], relations = [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]}
    it :: Poset
    (0.01 secs, 7774520 bytes)

Each int-poset could be expanded by satisfying reflexivity and transitivity properies:

    > expand $ Poset [1..7] [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]
    Poset {elements = [1,2,3,4,5,6,7], relations = [(1,1),(2,2),(3,3),(3,2),(3,1),(4,4),(4,2),(5,5),(5,4),(5,2),(5,3),(5,1),(6,6),(6,4),(6,2),(6,1),(7,7),(7,6),(7,4),(7,2),(7,1),(7,5),(7,3)]}
    it :: Poset

## Morphisms

    type Morph = IM.IntMap Int

Morphisms over a int-posets could be defined as a `map` over it's elements:

    fromList [(1,3),(2,4),(3,6),(4,1),(5,2),(6,3),(7,2)]
    it :: Morph

Also it could be defined by permutation list:

    > fromPerm [3,4,6,1,2,3,2]
    fromList [(1,3),(2,4),(3,6),(4,1),(5,2),(6,3),(7,2)]
    it :: Morph

Nice morphism over a poset is a morphism satisfying to isotone property, reducibility, idempotency and fixity:

    > let p = expand $ Poset [1..7] [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]
    p :: Poset

    > let m = fromPerm [1,2,3,4,5,6,7]
    m :: Morph

    > nice p m
    True
    it :: Bool

## Example

Find all nice morphisms over a given poset:

    Poset {elements = [1,2,3,4,5,6,7], relations = [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]}

It could be done by `generate` method:

    > mapM_ (print . toPerm) $ filter (nice p) $ generate (length $ elements p)
    [1,2,3,4,5,6,7]
    [3,3,3,5,5,7,7]
    [5,5,5,5,5,7,7]
    [6,6,7,6,7,6,7]
    [7,7,7,7,7,7,7]
    it :: ()

