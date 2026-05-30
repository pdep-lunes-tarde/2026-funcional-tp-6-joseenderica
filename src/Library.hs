module Library where
import PdePreludat

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | BaconDeTofu | Papas | PatiVegano | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pan = 2
precioIngrediente Panceta = 10
precioIngrediente Cheddar = 10
precioIngrediente Pollo =  10
precioIngrediente Curry = 5
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente BaconDeTofu = 12
precioIngrediente Papas = 10
precioIngrediente PanIntegral = 3
precioIngrediente PatiVegano = 10

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)

-- Parte I: Hamburguesas

cuartoDeLibra = Hamburguesa {
        precioBase=20,
        ingredientes=[Pan,Carne,Cheddar,Pan] }

precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa = foldl (+) (precioBase hamburguesa) (map precioIngrediente (ingredientes hamburguesa))

agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente ingrediente hamburguesa = Hamburguesa {precioBase=precioBase hamburguesa, ingredientes=(ingredientes hamburguesa)++[ingrediente]}

-- Así funciona bien, en el caso de que haya Carne y Pollo ya habrá agrandado con Carne
-- Si hay almenos uno de los ingredientes base a la hora de agrandar se suma ese. Una hamburguesa siempre debe tener un ingrediente base, sino somos una panadería.
agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa 
    |any (== Carne) (ingredientes hamburguesa) = agregarIngrediente Carne hamburguesa 
    |any (== Pollo) (ingredientes hamburguesa) = agregarIngrediente Pollo hamburguesa
    |any (== PatiVegano) (ingredientes hamburguesa) = agregarIngrediente PatiVegano hamburguesa

descuento :: Number -> Hamburguesa -> Hamburguesa
descuento porcentaje hamburguesa = 
    Hamburguesa {
        precioBase = (precioBase hamburguesa) * (1-(porcentaje/100)),
        ingredientes = ingredientes hamburguesa
                }

pdepBurger :: Hamburguesa
pdepBurger = (descuento 20 . agregarIngrediente Panceta . agregarIngrediente Cheddar . agrandar . agrandar) cuartoDeLibra

-- Parte II: Algunas hamburguesas más

dobleCuarto :: Hamburguesa
dobleCuarto = (agregarIngrediente Cheddar . agregarIngrediente Carne) cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep = agregarIngrediente Curry dobleCuarto

delDia :: Hamburguesa -> Hamburguesa
delDia = descuento 30 . agregarIngrediente Papas

--Parte III: algunos cambios más

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie hamburguesa = Hamburguesa {precioBase=precioBase hamburguesa,ingredientes=(map aux (ingredientes hamburguesa) ) }
     where
      aux ingrediente
         | (ingrediente==Carne)=PatiVegano
         | (ingrediente==Pollo)=PatiVegano
         | (ingrediente==Cheddar)=QuesoDeAlmendras
         | (ingrediente==Panceta)=BaconDeTofu
         | otherwise=ingrediente

cambiarPanDePati :: Hamburguesa -> Hamburguesa
cambiarPanDePati (Hamburguesa pb ing) = Hamburguesa pb (map (\x -> if (x==Pan) then PanIntegral else x) ing) 

dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = (cambiarPanDePati . hacerVeggie) dobleCuarto
