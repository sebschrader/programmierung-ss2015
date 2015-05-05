
module E03.A1 where

-- (a)
data TA = NilA | A TA TB

data TB = NilB | B TB TA

-- (b)
-- Idea: Two functions zahlBA to count the B nodes in a TA tree and zahlBB to
-- count the B nodes in a TB tree.
-- We define zahlB as zahlBA.
zahlB = zahlBA

zahlBA :: TA -> Int
zahlBA NilA = 0
zahlBA (A ta tb) = zahlBA ta + zahlBB tb

zahlBB :: TB -> Int
zahlBB NilB = 1
zahlBB (B tb ta) = zahlBB tb + zahlBA ta + 1

