{-# LANGUAGE NondecreasingIndentation #-}
module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "agrandar" $ do
        it "cada vez que se agranda una hamburguesa se agrega otro ingrediente base, en este caso se agrega una carne" $ do
            agrandar (Hamburguesa 20 [Carne]) `shouldBe` Hamburguesa 40 [Carne, Carne]
        it "cada vez que se agranda una hamburguesa se agrega otro ingrediente base, en este caso se agrega un pollo" $ do
            agrandar (Hamburguesa 10 [Pollo]) `shouldBe` Hamburguesa 20 [Pollo, Pollo]

        describe "agregarIngrediente" $ do
        it "recibe un ingrediente y lo agrega a la hamburguesa" $ do
            agregarIngrediente Panceta (Hamburguesa 24 [Pan, Carne, Pan]) `shouldBe` Hamburguesa 34 [Panceta, Pan, Carne, Pan] 

        describe "descuento" $ do
        it "aplica un descuento sobre el precio de la hamburguesa" $ do
            descuento 50 (Hamburguesa 24 [Pan, Carne, Pan]) `shouldBe` Hamburguesa 12 [Pan, Carne, Pan]
        it "aplica un descuento sobre el precio de la hamburguesa" $ do
            descuento 25 (Hamburguesa 44 [Pan, Carne, Panceta, Cheddar, Pan]) `shouldBe` Hamburguesa 33 [Pan, Carne, Panceta, Cheddar, Pan]
        

      
