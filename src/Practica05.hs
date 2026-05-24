module Practica05 where

import Terminos

--Aplicar una sustitucion a un termino
apsubT :: Term -> Subst -> Term
apsubT (Var nombre) s = buscar nombre s (Var nombre)
apsubT (Fun f args) s = Fun f (aplicarLista args s)

--Funcion auxiliar para aplicar la sustitucion a una lista de terminos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista [] _ = []
aplicarLista (t : resto) s = apsubT t s : aplicarLista resto s

--Funcion que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus [] = []
simpSus ((nombre, t) : resto)
    | t == Var nombre = simpSus resto
    | otherwise       = (nombre, t) : simpSus resto

--Funcion que calcula la composicion de dos sustituciones
compSus :: Subst -> Subst -> Subst
compSus = undefined

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
buscar :: Nombre -> Subst -> Term -> Term
buscar _ [] porDefecto = porDefecto
buscar nombre ((x, t) : resto) porDefecto
    | nombre == x = t
    | otherwise   = buscar nombre resto porDefecto
