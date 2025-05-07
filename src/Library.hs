{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Library where
import PdePreludat
import GHC.IO.Handle (hClose)

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

precioTotal :: Hamburguesa -> Number
precioTotal burguer = precioBase burguer + sumarPrecios (ingredientes burguer)

sumarPrecios :: [Ingrediente] -> Number
sumarPrecios [] = 0
sumarPrecios (cabeza:cola) = precioIngrediente cabeza + sumarPrecios cola

agrandar :: Hamburguesa -> Hamburguesa
agrandar burguer 
    |contiene Carne (ingredientes burguer) = agregarIngrediente Carne burguer
    |contiene Pollo (ingredientes burguer) = agregarIngrediente Pollo burguer
    |otherwise = burguer

contiene :: Ingrediente -> [Ingrediente] -> Bool
contiene _ [] = False
contiene ingrediente (cabeza:cola) 
    |ingrediente == cabeza = True
    |otherwise = contiene ingrediente cola

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa 
agregarIngrediente ing (Hamburguesa precio ings) = Hamburguesa (precio + precioIngrediente ing) (ing:ings)

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentajeDescuento (Hamburguesa precio ingredientes) = Hamburguesa (precio * (100 - porcentajeDescuento)/100) ingredientes 