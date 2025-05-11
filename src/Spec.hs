{-# LANGUAGE NondecreasingIndentation #-}
module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "agrandarPedido" $ do
        it "Agranda el pedido agregando un nuevo ingrediente, en este caso pollo" $ do
            agrandarPedido (Hamburguesa 14 [Pan,Pollo,Pan]) `shouldBe` Hamburguesa 24 [Pan,Pollo, Pollo, Pan]
        it "Agranda el pedido agregando un nuevo ingrediente, en este caso carne" $ do
            agrandarPedido (Hamburguesa 24 [Pan,Carne,Pan]) `shouldBe` Hamburguesa 44 [Pan,Carne,Carne,Pan]
        it "Agranda el pedido agregando un nuevo ingrediente, en este caso pollo o puede ser también carne" $ do
            agrandarPedido (Hamburguesa 34 [Pan,Carne,Pollo,Pan]) `shouldBe` Hamburguesa 54 [Pan,Carne,Carne,Pollo,Pan]

    describe "PdePBurguer" $ do
        it "Es un cuarto de libra agrandado 2 veces con panceta, cheddar y un 20% de descuento" $ do
            pdepBurger `shouldBe` Hamburguesa 110 [Pan,Panceta,Cheddar,Carne,Carne, Carne,Cheddar,Pan]
            
    describe "Hamburguesas" $ do
        it "Hamburguesa cuarto de libra con carne y cheddar" $ do
            dobleCuartoDeLibra `shouldBe` Hamburguesa 84 [Pan, Carne, Cheddar, Carne, Cheddar,Pan]
        it "Hamburguesa doble cuarto con curry" $ do
            bigPdep dobleCuartoDeLibra `shouldBe` Hamburguesa 89 [Pan,Curry, Carne, Cheddar, Carne, Cheddar,Pan]
        it "Promo hamburguesa con papas al 30% de descuento" $ do
            delDia dobleCuartoDeLibra  `shouldBe` Hamburguesa 88 [Pan, Papas, Carne, Cheddar, Carne, Cheddar,Pan]

    describe "Hacer un Veggie" $ do
        it "Cambiar Ingredientes para una persona vegana" $ do 
            hacerVeggie (Hamburguesa 54 [Pan,Carne,Pollo,Panceta,Cheddar,Pan]) `shouldBe` Hamburguesa 54 [Pan,PatiVegano,PatiVegano,BaconDeTofu,QuesoDeAlmendras,Pan]
        it "Cambiar Pan a Pan Integral " $ do
            cambiarPanDePati (Hamburguesa 54 [Pan,Carne,Pollo,Panceta,Cheddar,Pan]) `shouldBe` Hamburguesa 54 [PanIntegral,Carne,Pollo,Panceta,Cheddar,PanIntegral]
        it "Es un dobleCuarto veggie con pan integral" $ do
            dobleCuartoVegano (Hamburguesa 64 [Pan,Carne,Pollo,Panceta,Cheddar,Pan]) `shouldBe` Hamburguesa 64 [PanIntegral,PatiVegano,PatiVegano,BaconDeTofu,QuesoDeAlmendras,PanIntegral]
        
    
    

        
        

      
