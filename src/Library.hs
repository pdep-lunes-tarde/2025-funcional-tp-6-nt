{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat
import GHC.IO.Handle (hClose)

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu | PatiVegano | Papas | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10
precioIngrediente PanIntegral = 3

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

cuartoDeLibra = Hamburguesa { precioBase = 20, ingredientes = [Pan, Carne, Cheddar,Pan]}

agrandarPedido :: Hamburguesa -> Hamburguesa
agrandarPedido (Hamburguesa precioBase ingredientes)
    | esDeCarne ingredientes = Hamburguesa (precioBase + precioIngrediente Carne) (entrePanesDeCarne ingredientes)
    | not (esDeCarne ingredientes) = Hamburguesa (precioBase + precioIngrediente Pollo) (entrePanesDePollo ingredientes)
    | otherwise = Hamburguesa precioBase ingredientes


esDeCarne :: [Ingrediente] -> Bool
esDeCarne [] = False
esDeCarne (ingrediente : restoDeIngredientes) = ingrediente == Carne || esDeCarne restoDeIngredientes

entrePanesDePollo :: [Ingrediente] -> [Ingrediente]
entrePanesDePollo  [] = []
entrePanesDePollo losIngredientes = agregarALaHAmburguesa losIngredientes Pollo


entrePanesDeCarne :: [Ingrediente] -> [Ingrediente]
entrePanesDeCarne [] = []
entrePanesDeCarne losIngredientes = agregarALaHAmburguesa losIngredientes Carne

pdepBurger = descuento 20 . precioDeLaHamburguesa . agregarIngredientes Panceta . agregarIngredientes Cheddar . agrandarPedido . agrandarPedido $ cuartoDeLibra

precioDeLaPdep :: [Ingrediente]-> Number
precioDeLaPdep ingredientes = sum.map precioIngrediente $ ingredientes

agregarIngredientes :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngredientes  ingrediente (Hamburguesa precioBase losIngredientes) = Hamburguesa {precioBase = precioBase + precioIngrediente ingrediente, ingredientes = agregarALaHAmburguesa losIngredientes ingrediente }

descuento :: Number-> Hamburguesa -> Hamburguesa
descuento porcentaje unaHamburguesa = unaHamburguesa { precioBase = precioBase unaHamburguesa - precioBase cuartoDeLibra * porcentaje/100 }

precioDeLaHamburguesa :: Hamburguesa -> Hamburguesa
precioDeLaHamburguesa  unaHamburguesa = unaHamburguesa {precioBase =(+) (precioBase cuartoDeLibra). sum . map precioIngrediente  $ ingredientes unaHamburguesa, ingredientes = ingredientes unaHamburguesa}

agregarALaHAmburguesa :: [Ingrediente] -> Ingrediente -> [Ingrediente]
agregarALaHAmburguesa (x:xs) ingrediente = x:ingrediente:xs

dobleCuartoDeLibra :: Hamburguesa
dobleCuartoDeLibra  = precioDeLaHamburguesa . agrandarPedido . agregarIngredientes Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa -> Hamburguesa
bigPdep unaHamburguesa = precioDeLaHamburguesa . agregarIngredientes Curry $ dobleCuartoDeLibra

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 30 . precioDeLaHamburguesa . agregarIngredientes Papas $ hamburguesa



-- =================== Parte 3 ======================= 

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie (Hamburguesa precio ingredientes) =
    Hamburguesa precio $ map cambiarIngredientes ingredientes

cambiarIngredientes :: Ingrediente -> Ingrediente
cambiarIngredientes ingrediente
    | ingredienteBase ingrediente  =  PatiVegano
    | ingrediente == Cheddar =  QuesoDeAlmendras
    | ingrediente == Panceta =  BaconDeTofu
    | otherwise = ingrediente

ingredienteBase :: Ingrediente -> Bool
ingredienteBase Carne = True
ingredienteBase Pollo = True
ingredienteBase _ = False

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati (Hamburguesa precio ingredientes) =
    Hamburguesa precio $ map aPanIntegral ingredientes

aPanIntegral :: Ingrediente -> Ingrediente
aPanIntegral ingrediente
    | ingrediente == Pan = PanIntegral
    | otherwise = ingrediente

dobleCuartoVegano :: Hamburguesa -> Hamburguesa
dobleCuartoVegano hamburguesa = cambiarPanDePati . hacerVeggie $ hamburguesa