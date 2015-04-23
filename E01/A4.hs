module E01.A4 where
gen :: [Int]
gen = genFrom 1
  -- With where we can create definitions that are local to the line above and
  -- are not available outside of this line.
  where genFrom n = n:genFrom (n+1)

-- Haskell has also syntax for generating arbitrary list of enumerable items.
-- See https://hackage.haskell.org/package/base-4.8.0.0/docs/Prelude.html#t:Enum
gen' :: [Int]
gen' = [1..]

