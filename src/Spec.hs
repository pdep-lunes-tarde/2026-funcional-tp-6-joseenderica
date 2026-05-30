module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 6" $ do
        describe "Si los precios finales de todas las hamburguesas están bien, el programa es exitoso." $ do
            it "El precio final de una pdepBurger debería ser 110" $ do
                precioFinal pdepBurger `shouldBe` 110
            it "El precio final de una dobleCuarto debería ser 84." $ do
                precioFinal dobleCuarto `shouldBe` 84
            it "El precio final de una bigPdep debería ser 89, una ganga" $ do
                precioFinal bigPdep `shouldBe` 89
            it "El precio final de una dobleCuarto del día debería ser 88" $ do
                precioFinal (delDia dobleCuarto) `shouldBe` 88
            it "El precio final de un dobleCuartoVegano debería ser 76 (Más económico porque la carne cuesta el doble)" $ do
                precioFinal dobleCuartoVegano `shouldBe` 76

                    
            
