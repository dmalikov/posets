{-# LANGUAGE UnicodeSyntax #-}
import Control.Monad (forM_)

import Data.Poset
import Data.Poset.Morphism

{-
x = expand $ Poset [1..7] [(1,3),(1,6),(2,3),(2,4),(3,5),(4,5),(4,6),(5,7),(6,7)]
m = fromPerm [1,2,3,4,5,6,7]

main = do
  print $ isotone x m
  print $ reducible x m
  print $ idempotent x m
  print $ fixed x m
-}

examples ∷ [Morph]
examples = map fromPerm $
  [ [1,2,3,4,5,6,7]
  , [3,3,3,5,5,7,7]
  , [5,5,5,5,5,7,7]
  , [6,6,7,6,7,6,7]
  , [7,7,7,7,7,7,7]
  ]

po ∷ Poset
-- po = expand $ Poset [1..7] [(1,3),(1,6),(2,3),(2,4),(3,5),(4,5),(4,6),(5,7),(6,7)]
po = expand $ Poset [1..7] [(3,1),(3,2),(4,2),(5,3),(5,4),(6,1),(6,4),(7,5),(7,6)]

test_isotone ∷ Poset → IO ()
test_isotone p = forM_ examples $ \e → do
  putStrLn $ "isotone of " ++ show e ++ " - " ++ show (isotone p e)

test_reducible ∷ Poset → IO ()
test_reducible p = forM_ examples $ \e → do
  putStrLn $ "reducible of " ++ show e ++ " - " ++ show (reducible p e)

test_idempotent ∷ Poset → IO ()
test_idempotent p = forM_ examples $ \e → do
  putStrLn $ "idempotent of " ++ show e ++ " - " ++ show (idempotent p e)

test_fixed ∷ Poset → IO ()
test_fixed p = forM_ examples $ \e → do
  putStrLn $ "fixed of " ++ show e ++ " - " ++ show (fixed p e)

find_answers ∷ Poset → IO ()
find_answers p = print $ map toPerm $
  filter (nice p) $ generate $ length $ elements p

main = do
  test_isotone po
  test_reducible po
  test_idempotent po
  test_fixed po
  find_answers po
