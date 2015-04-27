
module E03.A1 where

-- (a)
data TA = NilA | A TA TB

data TB = NilB | B TB TA

-- (b)
zahlB :: TA -> Int
zahlB NilA = 0
zahlB (A ta tb) = zahlB ta + zahlBB tb

zahlBB :: TB -> Int
zahlBB NilB = 1
zahlBB (B tb ta) = zahlBB tb + zahlB ta + 1

