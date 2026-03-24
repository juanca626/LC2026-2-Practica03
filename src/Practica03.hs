module Practica03 where

import Data.List (nub, (\\)) --linea necesaria para ejecutar las pruebas unitarias y que funcionen

-- Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
          | And Prop Prop | Or Prop Prop
          | Impl Prop Prop | Syss Prop Prop
          deriving (Eq)

instance Show Prop where 
    show (Cons True) = "⊤"
    show (Cons False) = "⊥"
    show (Var p) = p
    show (Not p) = "¬" ++ show p
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

p, q, r, s, t, u, w, v :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
w = Var "w"
v = Var "v"

{-
FORMAS NORMALES
-}

-- Ejercicio 1
fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Cons b) = Cons b
fnn (And p q) = And (fnn p) (fnn q)
fnn (Or p q) = Or (fnn p) (fnn q)
-- condicionales y bicondicionales
fnn (Impl p q) = fnn (Or (Not p) q)
fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))
-- negaciones y dMorgan
fnn (Not (Var p)) = Not (Var p)
fnn (Not (Cons b)) = Cons (not b)
fnn (Not (Not p)) = fnn p
fnn (Not (And p q)) = Or (fnn (Not p)) (fnn (Not q))
fnn (Not (Or p q)) = And (fnn (Not p)) (fnn (Not q))
fnn (Not (Impl p q)) = And (fnn p) (fnn (Not q))
fnn (Not (Syss p q)) = fnn (Not (And (Impl p q) (Impl q p)))

-- Ejercicio 2
fnc :: Prop -> Prop
fnc p' = dist (fnn p')
  where
    dist (And p1 p2) = And (dist p1) (dist p2)
    dist (Or p1 p2)  = distOr (dist p1) (dist p2)
    dist prop        = prop
    distOr p1 (And p2 r) = And (distOr p1 p2) (distOr p1 r)
    distOr (And p1 p2) r = And (distOr p1 r) (distOr p2 r)
    distOr p1 p2         = Or p1 p2

{-
RESOLUCION BINARIA
-}

-- sinonimos a usar
type Literal = Prop
type Clausula = [Literal]

-- auxiliar para obtener el complemento de un literal
comp :: Literal -> Literal
comp (Var x) = Not (Var x)
comp (Not (Var x)) = Var x
comp l = l

-- Ejercicio 1
clausulas :: Prop -> [Clausula]
clausulas prop = getClausulas (fnc prop)
  where
    getClausulas (And p1 p2) = getClausulas p1 ++ getClausulas p2
    getClausulas p1          = [nub (getLiterals p1)]
    
    getLiterals (Or p1 p2) = getLiterals p1 ++ getLiterals p2
    getLiterals p1         = [p1]

-- Ejercicio 2
resolucion :: Clausula -> Clausula -> Clausula
resolucion c1 c2 = 
    case [(l1, l2) | l1 <- c1, l2 <- c2, l1 == comp l2] of
        []           -> nub (c1 ++ c2)
        ((l1, l2):_) -> nub (filter (/= l1) c1 ++ filter (/= l2) c2)

{-
ALGORITMO DE SATURACION
-}

-- Ejercicio 1
hayResolvente :: Clausula -> Clausula -> Bool
hayResolvente c1 c2 = any (\l -> comp l `elem` c2) c1

-- funcion recursiva vista en clase que nos ayuda a obtener las nuevas resoluciones
rs :: [Clausula] -> [Clausula]
rs [] = []
rs [x] = [x]
rs (x:(y:xs)) = 
    if hayResolvente x y
        then (x:(y:xs)) ++ [resolucion x y] ++ rs (x:xs) ++ rs (y:xs)
        else (x:(y:xs)) ++ rs (x:xs) ++ rs (y:xs)

-- Ejercicio 2
saturacion :: Prop -> Bool
saturacion = undefined