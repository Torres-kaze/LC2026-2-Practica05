module Practica05 where

import Terminos

--Aplicar una sustitucion a un termino
apsubT :: Term -> Subst -> Term
apsubT = undefined 

--Funcion auxiliar para aplicar la sustitucion a una lista de terminos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista = undefined 

--Funcion que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus = undefined

-- Composicion de dos sustituciones
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = simpSus (concatenar parte1 parte2)
  where
    parte1 = aplicarS2 s1 s2
    parte2 = filtrarNuevos s2 (dominio s1)

--Funcion que devuelve un umg de dos terminos, si es que lo hay
unifica :: Term -> Term -> [Subst]
unifica = undefined


--Funcion que devuelve un unificador de dos términos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas = undefined

--Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj = undefined


-- Funciones auxiliares
aplicarS2 :: Subst -> Subst -> Subst
aplicarS2 [] _ = []
aplicarS2 ((nombre, t) : resto) s2 =
    (nombre, apsubT t s2) : aplicarS2 resto s2

filtrarNuevos :: Subst -> [Nombre] -> Subst
filtrarNuevos [] _ = []
filtrarNuevos ((nombre, t) : resto) dom
    | estaEn nombre dom = filtrarNuevos resto dom
    | otherwise         = (nombre, t) : filtrarNuevos resto dom

estaEn :: Nombre -> [Nombre] -> Bool
estaEn _ [] = False
estaEn nombre (x : resto)
    | nombre == x = True
    | otherwise   = estaEn nombre resto

dominio :: Subst -> [Nombre]
dominio []                    = []
dominio ((nombre, _) : resto) = nombre : dominio resto

concatenar :: [a] -> [a] -> [a]
concatenar [] ys       = ys
concatenar (x : xs) ys = x : concatenar xs ys
