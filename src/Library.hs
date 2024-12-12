module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Elemento = UnElemento { 
    tipo :: String,
    ataque :: Ataque,
    defensa :: Defensa 
} deriving(Eq, Show)

data Personaje = UnPersonaje { 
    nombre :: String,
    salud :: Number,
    elementos :: [Elemento],
    anioPresente :: Number 
} deriving(Eq, Show)

type Ataque = Personaje -> Personaje

type Defensa = Personaje -> Personaje

-- Punto 1

mandarAlAnio :: Number -> Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

modificarSalud :: (Number -> Number) -> Personaje -> Personaje
modificarSalud funcion personaje = personaje {salud = funcion (salud personaje)}

meditar :: Personaje -> Personaje
meditar = modificarSalud (* 1.5)

causarDanio :: Number -> Personaje -> Personaje
causarDanio n = modificarSalud ((max 0).(+ (-n)))

-- Punto 2

esDeTipo :: String -> Elemento -> Bool
esDeTipo tipoBuscado = (== tipoBuscado).tipo

esMalvado :: Personaje -> Bool
esMalvado personaje = any (esDeTipo "Maldad") (elementos personaje)

atacar :: Elemento -> Personaje -> Personaje
atacar elemento = (ataque elemento)

danioQueProduce :: Personaje -> Elemento -> Number
danioQueProduce personaje elemento = (salud personaje) - ((salud.(ataque elemento)) personaje)

elementoMortal :: Elemento -> Personaje -> Bool
elementoMortal elemento = (== 0).salud.(ataque elemento)

enemigoMortal :: Personaje -> Personaje -> Bool
enemigoMortal personaje enemigo = any (flip elementoMortal personaje) (elementos enemigo)

enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales personaje = filter (enemigoMortal personaje)

-- Punto 3

nada :: Personaje -> Personaje
nada = id

meditaciones :: Number -> Defensa
meditaciones n = (flip (!!) n).(iterate meditar)

concentracion :: Number -> Elemento
concentracion nivel = UnElemento "Magia" nada (meditaciones nivel)

esbirroMalvado :: Elemento
esbirroMalvado = UnElemento "Maldad" (causarDanio 1) nada

esbirrosMalvados :: Number -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad esbirroMalvado

katanaMagica :: Elemento
katanaMagica = UnElemento "Magia" (causarDanio 1000) nada

jack :: Personaje
jack = UnPersonaje "Jack" 300 [(concentracion 3), katanaMagica] 200

portalAlFuturo :: Number -> Elemento
portalAlFuturo anio = UnElemento "Magia" (mandarAlAnio (anio + 2800)) ((aku (anio + 2800)).salud)

aku :: Number -> Number -> Personaje
aku anio cantSalud = UnPersonaje "Aku" cantSalud ([(concentracion 4), (portalAlFuturo anio)] ++ (esbirrosMalvados (3 * anio))) anio

aku1 :: Personaje
aku1 = aku 2 600

-- Punto 4

estaMuerto :: Personaje -> Bool
estaMuerto = (== 0).salud

defensas :: [Elemento] -> [Defensa]
defensas = map defensa

ataques :: [Elemento] -> [Ataque]
ataques = map ataque

aplicarDefensa :: [Elemento] -> Personaje -> Personaje
aplicarDefensa lista personaje = foldr ($) personaje (defensas lista)

aplicarAtaques :: [Elemento] -> Personaje -> Personaje
aplicarAtaques lista personaje = foldr ($) personaje (ataques lista)

defenderse :: Personaje -> Personaje
defenderse personaje = aplicarDefensa (elementos personaje) personaje

serAtacado :: Personaje -> Personaje -> Personaje
serAtacado atacante atacado = aplicarAtaques (elementos atacante) atacado

luchar :: Personaje -> Personaje -> (String, String)
luchar atacante defensor
    | estaMuerto atacante = ((nombre defensor), (nombre atacante))
    | otherwise = luchar (serAtacado atacante defensor) (defenderse atacante)