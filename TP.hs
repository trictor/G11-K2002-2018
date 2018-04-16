{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe 
import Test.Hspec

--DATA--
data Usuario = Usuario {
 nombre :: String, 
 billetera :: Float
} deriving (Show, Eq)


--USUARIOS--
usuarioDePrueba = Usuario "" 10
usuarioDePrueba2 = Usuario "" 20
usuarioDePrueba3 = Usuario "" 50

pepe = Usuario "Jose:" 10
lucho = Usuario "Luciano" 2
pepe2 = Usuario "pepe" 20


--TESTS--
tests = hspec $ do
  describe "Primera parte" $ do
  it "1)Si a una billetera de 10 monedas se le depositan 10 monedas queda con 20 monedas"$ (deposito 10 usuarioDePrueba) `shouldBe` (usuarioDePrueba {billetera = 20})
  it "2)Si a una billetera de 10 monedas se le extraen 3 monedas queda con 7 monedas" $ (extraccion 3 usuarioDePrueba) `shouldBe` (usuarioDePrueba {billetera = 7})
  it "3)si a una billetera de 10 monedas se le extraen 15 monedas queda con 0 monedas " $ (extraccion 15 usuarioDePrueba) `shouldBe` (usuarioDePrueba {billetera = 0})
  it "4)Si a una billetera de 10 monedas se le realiza un upgrade queda con 12 monedas" $ (upgrade usuarioDePrueba) `shouldBe` (usuarioDePrueba {billetera = 12})
  it "5)Si a una billetera de 10 monedas la cerras queda en 0 monedas" $ (cierreDeCuenta usuarioDePrueba) `shouldBe` (usuarioDePrueba {billetera = 0})
  it "6)Si a una billetera de 10 monedas queda igual entonces queda con 20 monedas" $ (quedaIgual usuarioDePrueba) `shouldBe` (usuarioDePrueba {billetera = 10})
  it "7)Si a una billetera de 10 monedas se le depositan 1000 monedas y se le hace un upgrade queda con 1020 monedas" $ ((upgrade.(deposito 1000)) usuarioDePrueba) `shouldBe` (usuarioDePrueba {billetera = 1020})
  describe "Segunda parte" $ do  
  it "8)La billetera de Pepe debe ser de 10 monedas" $ (billetera pepe `shouldBe` 10)
  it "9)La billetera de Pepe luego de un cierre de cuenta es 0" $ (cierreDeCuenta pepe) `shouldBe` (pepe {billetera = 0})
  it "10)La billetera de Pepe luego de que le depositen 15 monedas, extraigan 2 y tenga un Upgrade queda con 27.6 monedas" $ ((upgrade.(extraccion 2).(deposito 15)) pepe) `shouldBe` (pepe {billetera = 27.6})
 




--TIPOS--
type Evento = Usuario -> Usuario
deposito :: Float -> Evento
extraccion :: Float -> Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento



--EVENTOS--
nuevoMonto nMonto usuario = usuario {billetera = nMonto}

extraccionSirve dineroDepositado usuario | billetera usuario > dineroDepositado = (billetera usuario - dineroDepositado)
                                         | billetera usuario <= dineroDepositado = 0

upgradeSirve usuario | ((billetera usuario * 1.2) - billetera usuario ) < 10 = (billetera usuario * 1.2)
                     | (billetera usuario * 1.2) > 10 = (billetera usuario + 10)


deposito dineroDepositado usuario = nuevoMonto (billetera usuario + dineroDepositado) usuario
extraccion dineroAExtraer usuario = nuevoMonto (extraccionSirve dineroAExtraer usuario) usuario
upgrade usuario = nuevoMonto (upgradeSirve usuario ) usuario
cierreDeCuenta usuario = nuevoMonto 0 usuario
quedaIgual usuario = usuario
 
