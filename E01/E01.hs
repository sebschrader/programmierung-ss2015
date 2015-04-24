-- Übung erarbeitet Di 1. DS Tutor Peter Klausing
-- peter.klausing@mailbox.tu-dresden.de



-- Links
-- https://github.com/sebschrader/programmierung-ss2015
-- https://piratenpad.de/p/M7s2sGAY
-- http://learnyouahaskell.com
-- http://tryhaskell.org/

-- Kommentar
-- Am Anfang jeder Datei angeben. Muss mit Großbuchstaben starten
module E01 where

-- Funktionen definieren
-- Beispiel Fakultätsfunktion
-- Fak: N -> N 
-- Fak(n)= 1 für n == 1
--         n * Fak(n-1) für alle anderen  

-- mittels Patternmatching
-- Funktionsname :: Bildet ab von Int auf Int auf Int ab. 
fak :: Int -> Int
fak 1 = 1
fak n = n * fak (n-1)

-- fak 2 = 2 * fak (2 - 1) = 2 * fak 1 = 2 * 1

-- mittels if then else
fak' :: Int -> Int
fak' n = if n == 1 then 1 else  n * fak' (n-1)

-- Add soll Int Int nehmen und addieren
add :: Int -> Int -> Int
add a b = a + b

-- square1 quadriert eine Int
square1 :: Int -> Int
square1 n = n*n

-- Listen-Beispiele
-- [1,2,3]
-- 1:2:3:[] = [1,2,3]
-- [1,2] ++ [3]
-- head : erstes Element einer Liste
-- tail : Liste ohne erstes Element
-- last : letztes Element
-- init : Liste ohne letztes Element
-- length : Länge der Liste
-- null : Bool  gibt True zurück falls Liste leer []
-- reverse : Liste umkehren
-- take : nimmt ersten n Element einer Liste

-- Listen in Funktionen 
-- [Int] : Liste von Integer
-- [[Int]] : Liste von Listen von Ints 
-- (x:xs) : head : tail

-- 1
-- a
square :: [Int] -> [Int]
square [] = []
square (x:xs) = (square1 x) : (square xs)

-- b
at :: [Int] -> Int -> Int
at [] _ = error "error"
at (x:xs) 0 = x
at (x:xs) n = at xs (n-1)
-- if then else

-- c
dup :: [Int] -> [Int] -> [Int]
dup [] _ = []
dup _ [] = []
dup (x:xs) ys 
    | x `isin` ys = x:(dup xs ys)
    | otherwise = dup xs ys
    where isin :: Int -> [Int] -> Bool
          isin _ [] = False 
          isin x (y:ys) = if x == y then True else x `isin` ys
    
-- 2
f :: [Int] -> [Int]
f [] = []
f (x:xs)
    | x > 0 = f xs ++ [x]
    | otherwise = (f xs)

-- 3 
-- a Beispiel konstruieren
bsp = [[2,2,3,2],[22,31,3,34],[21,3]]
-- b 
-- trivial
max_length :: [[Int]] -> Int
max_length [] = 0
max_length (x:xs) = length x `max` max_length xs

-- mittels map
max_length' :: [[Int]] -> Int
max_length' xs = maximum (map d xs)
    where d :: [Int] -> Int
          d x = length x

-- einfach Implementierung von Int
map' :: (Int -> Int) -> [Int] -> [Int]
map' f [] = []
map' f (x:xs) = f x : map f xs 

  
-- 4 unendliche Listen
-- a einstellige Funktionen

gen :: [Int]
gen = gen' 1
    where gen' :: Int -> [Int]
          gen' p = p : gen' (p + 1) 

gen'' :: [Int]
gen'' = [1,2..]

-- nullstellige Funktionen -> keine Argumente
pii :: Float
pii = 3.14

greatnumber :: Integer
greatnumber = 43274981237586231785638246918724545243

-- b
-- Erinnerung
-- at :: [Int] -> Int -> Int
-- at [] _ = error "error"
-- at (x:xs) 0 = x
-- at (x:xs) n = at xs (n-1)

-- at gen 2 = at (gen' 1) 2
--          = at (1 : gen' (1+1)) 2 & xs=gen'(1+1)
--          = at (gen' 2) 1
--          = at (2: gen' (2 +1)) 1
--          = at (gen' 3) (0)
--          = at (3 : gen' (3+1)) 0
--          = 3
