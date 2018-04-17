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
usuarioDePrueba1 = Usuario "" 10
usuarioDePrueba2 = Usuario "" 20
usuarioDePrueba3 = Usuario "" 50

pepe = Usuario "Jose" 10
lucho = Usuario "Luciano" 2
pepe2 = Usuario "Jose" 20


--TESTS--
testing = hspec $ do
 describe "Eventos" $ do
  it "1) A una billetera de 10 monedas se le depositan 10 monedas, entonces queda con 20 monedas."$ deposito 10 usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 20} -- COMPARAR BILLETERA CON FLOAT
  it "2) A una billetera de 10 monedas se le extraen 3 monedas, entonces queda con 7 monedas." $ extraccion 3 usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 7}
  it "3) A una billetera de 10 monedas se le extraen 15 monedas, entonces queda con 0 monedas." $ extraccion 15 usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 0}
  it "4) A una billetera de 10 monedas se le realiza un upgrade, entonces queda con 12 monedas." $ upgrade usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 12}
  it "5) A una billetera de 10 monedas la cerras, entonces queda con 0 monedas." $ cierreDeCuenta usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 0}
  it "6) Una billetera de 10 monedas queda igual, entonces queda con 10 monedas." $ quedaIgual usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 10}
  it "7) A una billetera de 10 monedas se le depositan 1000 monedas y se le hace un upgrade,entonces queda con 1020 monedas." $ (upgrade.deposito 1000) usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 1020}
 describe "Usuarios" $ do  
  it "8) La billetera de Pepe es de 10 monedas." $ billetera pepe `shouldBe` 10
  it "9) La billetera de Pepe luego de un cierre de cuenta es de 0 monedas." $ cierreDeCuenta pepe `shouldBe` pepe {billetera = 0}
  it "10) La billetera de Pepe luego de que le depositen 15 monedas, extraigan 2 y tenga un Upgrade es 27.6 monedas." $ (upgrade.extraccion 2.deposito 15) pepe `shouldBe` pepe {billetera = 27.6}
 describe "Transacciones" $ do
  it "11) Se aplica la transaccion 1 a Pepe, produciendose el evento 'quedaIgual', que si se aplica a una billetera de 20 monedas da como resultado 20 monedas." $ transaccion1 pepe usuarioDePrueba2 `shouldBe` usuarioDePrueba2 {billetera = 20}
  it "12) Se aplica la transaccion 2 a Pepe, produciendose el evento 'deposito 5', que si se aplica a una billetera de 10 monedas da como resultado 15 monedas." $ transaccion2 pepe pepe `shouldBe` pepe {billetera = 15}
  it "13) Se aplica la transaccion 2 a Pepe2, produciedose el evento 'desposito 5', que si se aplica a una billetera de 50 monedas da como resultado 55 monedas." $ transaccion2 pepe2 usuarioDePrueba3 `shouldBe` usuarioDePrueba3 {billetera = 55}
 describe "Nuevos Eventos" $ do
  it "14) Se aplica la transaccion 3 a Lucho, produciendose el evento 'tocoYMeVoy', que si se aplica a una billetera de 10 monedas da como resultado 0 monedas." $ transaccion3 lucho usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 0}
  it "15) Se aplica la transaccion 4 a Lucho, produciendose el evento 'ahorranteErrante', que si se aplica a una billetera de 10 monedas da como resultado 34 monedas." $ transaccion4 lucho usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 34}
 describe "Pagos entre Usuarios" $ do
  it "16) Se aplica la transaccion 5 a Pepe, produciendose el evento 'extraccion 7', que si se aplica a una billetera de 10 monedas da como resultado 3 monedas." $ transaccion5 pepe usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 3}
  it "17) Se aplica la transaccion 5 a Lucho, produciendose el evento 'deposito 7', que si se aplica a una billetera de 10 monedas da como resultado 17 monedas." $ transaccion5 lucho usuarioDePrueba1 `shouldBe` usuarioDePrueba1 {billetera = 17}

--TIPOS--
type Evento = Usuario -> Usuario
deposito :: Float -> Evento
extraccion :: Float -> Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento
tocoYMeVoy :: Evento
ahorranteErrante :: Evento
transaccion1 :: Usuario -> Evento
transaccion2 :: Usuario -> Evento
transaccion3 :: Usuario -> Evento
transaccion4 :: Usuario -> Evento
transaccion5 :: Usuario -> Evento

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
quedaIgual usuario = usuario -- id
tocoYMeVoy usuario = (cierreDeCuenta.upgrade.deposito 5) usuario --POINT FREE
ahorranteErrante usuario=(deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1) usuario --POINT FREE


--TRANSACCIONES-- --HACER UNA FUNCION QUE GENERE TRANSACCIONES EVENTO-USUARIOAAPLICAR-USUARIOAVERIFICAR-EVENTO
transaccion1 usuario | nombre usuario == "Luciano" = cierreDeCuenta --ABSTRAER A FUNCION COMPARAR USUARIOS
                     | otherwise = quedaIgual 

transaccion2 usuario | nombre usuario == "Jose" = deposito 5
                     | otherwise = quedaIgual

transaccion3 usuario | nombre usuario == "Luciano" = tocoYMeVoy 
                     | otherwise = quedaIgual 

transaccion4 usuario | nombre usuario == "Luciano" = ahorranteErrante 
                     | otherwise = quedaIgual 

transaccion5 usuario | nombre usuario == "Jose" = extraccion 7
                     | nombre usuario == "Luciano" = deposito 7
                     | otherwise = quedaIgual 
