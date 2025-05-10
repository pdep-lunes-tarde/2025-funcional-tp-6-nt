{-# LANGUAGE NondecreasingIndentation #-}
module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "agrandarPedido" $ do
        it "el culito de nico esta bien rico" $ do
            agrandarPedido (Hamburguesa 14 [Pan,Pollo,Pan]) `shouldBe` Hamburguesa 24 [Pan,Pollo, Pollo, Pan]
        it "el culito de nico esta bien rico" $ do
            agrandarPedido (Hamburguesa 24 [Pan,Carne,Pan]) `shouldBe` Hamburguesa 44 [Pan,Carne,Carne,Pan]
        it "el culito de nico esta bien rico" $ do
            agrandarPedido (Hamburguesa 34 [Pan,Carne,Pollo,Pan]) `shouldBe` Hamburguesa 54 [Pan,Carne,Carne,Pollo,Pan]

    describe "PdePBurguer" $ do
        it "el culito de nico esta bien rico 2" $ do
            pdepBurger `shouldBe` Hamburguesa 110 [Pan,Panceta,Cheddar,Carne,Carne, Carne,Cheddar,Pan]

    describe "Hamburguesas" $ do
        it "el culito de nico esta bien rico 2" $ do
            dobleCuartoDeLibra `shouldBe` Hamburguesa 84 [Pan, Carne, Cheddar, Carne, Cheddar,Pan]
        it "el culito de nico esta bien rico 2" $ do
            bigPdep dobleCuartoDeLibra `shouldBe` Hamburguesa 89 [Pan,Curry, Carne, Cheddar, Carne, Cheddar,Pan]
        it "el culito de nico esta bien rico 2" $ do
            delDia cuartoDeLibra  `shouldBe` Hamburguesa 44.8 [Pan,Curry, Carne, Cheddar, Carne, Cheddar,Pan]
        
    
    

        
        

      
