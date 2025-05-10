{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat
import GHC.IO.Handle (hClose)

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Papas = 10

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

pdepBurger = Hamburguesa { precioBase = precioDeLaPdep [Pan,Panceta,Cheddar,Carne, Carne,Carne,Cheddar,Pan] , ingredientes = [Pan,Panceta,Cheddar,Carne, Carne,Carne,Cheddar,Pan]}

precioDeLaPdep :: [Ingrediente]-> Number
precioDeLaPdep ingredientes = sum.map precioIngrediente $ ingredientes

agregarIngredientes :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngredientes  ingrediente (Hamburguesa precioBase losIngredientes) = Hamburguesa {precioBase = precioBase + precioIngrediente ingrediente, ingredientes = agregarALaHAmburguesa losIngredientes ingrediente }

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcDesc (Hamburguesa precioBase ingredientes) = Hamburguesa {precioBase = precioBase - (precioBase * porcDesc) / 100, ingredientes = ingredientes}

agregarALaHAmburguesa :: [Ingrediente]->Ingrediente -> [Ingrediente]
agregarALaHAmburguesa (x:xs) ingrediente = x:ingrediente:xs

dobleCuartoDeLibra :: Hamburguesa 
dobleCuartoDeLibra  = agregarIngredientes Carne . agregarIngredientes Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa -> Hamburguesa
bigPdep unaHamburguesa = agregarIngredientes Curry dobleCuartoDeLibra

delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = descuento 30 . agregarIngredientes Papas $ hamburguesa


-- ### Parte 1: Hamburguesas

-- Nos pusimos una hamburguesería y queremos implementar un sistema para calcular los precios de las hamburguesas que vamos a vender. Cada hamburugesa tiene un precio base y una lista de ingredientes.

-- El precio final de una hamburguesa es la sumatoria de los precios de los ingredientes + el precio base.

-- - Carne = 20
-- - Pan = 2
-- - Panceta = 10
-- - Cheddar = 10
-- - Pollo = 10
-- - Curry = 5
-- - QuesoDeAlmendras = 15
-- - BaconDeTofu = 12

-- Un ejemplo de hamburguesa es el **cuartoDeLibra**, una hamburguesa de pan, carne, cheddar, pan y su precio base es de 20.

-- - la **pdepBurger**, que es un cuarto de libra agrandado 2 veces con panceta, cheddar y 20% de descuento. Su precio final deberia ser 110.

-- ### PARTE 2: Algunas hamburguesas más
-- El negocio se agrandó y queremos agregar las siguientes hamburguesas:
-- - **dobleCuarto** = es un cuarto de libra con carne y cheddar. El precio final deberia ser 84.
-- - **bigPdep** = es un doble cuarto con curry. El precio final deberia ser 89.
-- - **delDia** = es una promo que, dada una hamburguesa, le agrega Papas y un descuento del 30%. Por ejemplo, podría pedir una big pdep del dia y debería ser como una big pdep (doble cuarto con curry) pero con papas y el descuento del 30%. Por ejemplo una doble cuarto del día deberia valer 88.

-- Las papas son un ingrediente que cuesta $10.

-- ### PARTE 3: algunos cambios más 

-- Queremos modelar los siguientes modificadores:
-- - **hacerVeggie** : cambia todos los ingredientes base que hayan en la hamburguesa por PatiVegano (nuevo ingrediente base, también de precio 10), el cheddar lo cambia por queso de almendras y la panceta por bacon de tofu.

--   _Nota: ahora que hay un nuevo ingrediente base, **agrandar** una hamburguesa debería modificarse de manera acorde. Es decir, agrandar una hamburguesa cuyo ingrediente base es un PatiVegano debería agregarle otro PatiVegano._
-- - **cambiarPanDePati** : cambia el Pan que haya en la hamburguesa por PanIntegral (ingrediente de precio 3).
-- - hacer el **dobleCuartoVegano**, que es un dobleCuarto veggie con pan integral.


