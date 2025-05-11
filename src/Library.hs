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

--precioIngredientesCuartoDeLibra :: Hamburguesa -> Number
--precioIngredientesCuartoDeLibra (Hamburguesa precioBase ingredientes) = precioBaseCuartoDeLibra + sum (map precioIngrediente ingredientes) 


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

pdepBurger = Hamburguesa { precioBase = 110    , ingredientes = [Pan,Panceta,Cheddar,Carne, Carne,Carne,Cheddar,Pan]}

precioDeLaPdep :: [Ingrediente]-> Number
precioDeLaPdep ingredientes = sum.map precioIngrediente $ ingredientes

agregarIngredientes :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngredientes  ingrediente (Hamburguesa precioBase losIngredientes) = Hamburguesa {precioBase = precioBase + precioIngrediente ingrediente, ingredientes = agregarALaHAmburguesa losIngredientes ingrediente }

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcDesc (Hamburguesa precioBase ingredientes) = Hamburguesa {precioBase = precioBase - (precioBase * porcDesc) / 100, ingredientes = ingredientes}

agregarALaHAmburguesa :: [Ingrediente] -> Ingrediente -> [Ingrediente]
agregarALaHAmburguesa (x:xs) ingrediente = x:ingrediente:xs

dobleCuartoDeLibra :: Hamburguesa
dobleCuartoDeLibra  = agregarIngredientes Carne . agregarIngredientes Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa -> Hamburguesa
bigPdep unaHamburguesa = agregarIngredientes Curry dobleCuartoDeLibra

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 30 . agregarIngredientes Papas $ hamburguesa

--precioBaseCuartoDeLibra :: Number
--precioBaseCuartoDeLibra = 20

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