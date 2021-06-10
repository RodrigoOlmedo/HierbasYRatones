module Library where
import PdePreludat

data Raton = UnRaton {
    nombre :: String
,   edad :: Number
,   peso :: Number
,   enfermedades :: [String]
} deriving (Show,Eq)

{-
Cerebro es un ratón con 9 años, 0.2 kg de peso y tiene brucelosis, sarampión y tuberculosis.
Bicenterrata es un ratón con 256 años, 0.2kg de peso, y completamente sano.
Huesudo es un ratón de 4 años con 10kg de peso, y alta obesidad y sinusitis
-}

cerebro = UnRaton{
    nombre = "cerebro"
,   edad = 9
,   peso = 0.2
,   enfermedades = ["brucelosis", "sarampion", "tuberculosis"]
}
bicenterrata = UnRaton{
    nombre = "bicenterrata"
,   edad = 256
,   peso = 0.2
,   enfermedades = []
}
huesudo = UnRaton{
    nombre = "huesudo"
,   edad = 4
,   peso = 10
,   enfermedades = ["alta obesidad", "sinusitis"]
}

type Hierba = Raton->Raton

hierbaBuena :: Hierba
hierbaBuena = rejuvenecer sqrt

rejuvenecer :: (Number->Number)-> Raton -> Raton
rejuvenecer f raton = raton {edad = f (edad raton) }

hierbaVerde :: String->Hierba
hierbaVerde terminacion raton = eliminarEnfermedades (((enfermedadesQueTerminanEn terminacion).enfermedades) raton) raton

enfermedadesQueTerminanEn :: String-> [String] -> [String]
enfermedadesQueTerminanEn terminacion = filter (terminaEn terminacion)

terminaEn :: (Eq a)=> [a]-> [a] -> Bool
terminaEn terminacion = (==terminacion).(takeLast (length terminacion)) 

takeLast :: Number->[a]->[a]
takeLast numero = reverse.(take numero).reverse

eliminarEnfermedades :: [String]->Raton->Raton
eliminarEnfermedades enfermedadesAEliminar raton = raton {enfermedades = ((eliminarElementosDeLista enfermedadesAEliminar).enfermedades) raton}

eliminarElementosDeLista :: (Eq a) => [a] -> [a]-> [a]
eliminarElementosDeLista [] lista = lista
eliminarElementosDeLista (x:xs) lista = eliminarElementosDeLista xs (delete x lista)

delete :: (Eq a) => a -> [a]->[a]
delete _ [] = []
delete x (y:ys) | x == y    = delete x ys
                              | otherwise = y : delete x ys

{-
alcachofa, hace que el ratón pierda peso en un 10% si pesa más de 2kg, sino pierde un 5%.
Por ejemplo, un raton de 3 kg queda con 2,7 kg y cerebro queda con 0.19 kg. 
-}

alcachofa :: Hierba
alcachofa raton | pesaMasDe 2 (peso raton) = perderPeso (0.1*(peso raton)) raton
                | otherwise = perderPeso (0.05*(peso raton)) raton

pesaMasDe :: Number -> Number -> Bool
pesaMasDe  total cantidad = total < cantidad 

perderPeso :: Number -> Raton -> Raton
perderPeso cantidad raton = raton {peso = max ((peso raton)-cantidad) 0}
    
{-
hierbaZort, hace que el ratón se transforme en Pinky, perdiendo todas sus enfermedades y quedando con 0 años de edad.
-}

hierbaZort :: Hierba
hierbaZort = transformarseEnPinky

transformarseEnPinky :: Raton -> Raton
transformarseEnPinky raton = raton {nombre="pinky", enfermedades = [], edad = 0}

{-
hierbaDelDiablo, hace que el ratón pierda 0.1kg (sin disminuir de 0) y elimina todas las enfermedades con menos de 10 letras. 
-}

hierbaDelDiablo :: Hierba
hierbaDelDiablo raton = ((perderPeso 0.1).(eliminarEnfermedades ((enfermedadesConMenosDe10Letras.enfermedades) raton))) raton

enfermedadesConMenosDe10Letras :: [String]->[String]
enfermedadesConMenosDe10Letras = filter (menosDeTantasLetras 10)

menosDeTantasLetras :: Number -> String -> Bool
menosDeTantasLetras cantLetras = (>cantLetras).length

--Medicamentos 
{-
Hacer el pondsAntiAge, que es un medicamento que está hecho con 3 hierbas buenas y una alcachofa. iterate
Por ejemplo, si se lo administramos al ratón Bicenterrata, queda con 2 años y 0.19 kg 
-}
type Medicamento = Raton -> Raton

pondsAntiAge :: Medicamento
pondsAntiAge = aplicarHierbas [hierbaBuena,hierbaBuena,hierbaBuena,alcachofa]

{-
Hacer el reduceFatFast, (que viene en distintas potencias) y es un medicamento compuesto por una hierbaVerde de “obesidad” y tantas alcachofas 
como indique su potencia.
Por ejemplo administrándole a Huesudo un reduceFatFast de potencia 1 hace que huesudo pase a pesar 9 kg y sólo quede con sinusitis. Si en lugar
 de la 1 le administramos un reduceFatFast de potencia 2, pasa a pesar 8.1 kg y queda también solo con sinusitis.

-}
reduceFatFast :: Number->Medicamento
reduceFatFast potencia = aplicarHierbas ((replicate potencia alcachofa)++[hierbaVerde "obesidad"])

aplicarHierbas :: [Hierba]->Raton->Raton
aplicarHierbas hierbas raton= foldl aplicarHierba raton hierbas

aplicarHierba :: Raton -> Hierba -> Raton
aplicarHierba raton hierba = hierba raton
{-

Hacer la pdepCilina, que es un medicamento que usa hierbasVerdes para curar todas las enfermedades infecciosas. Las enfermedades infecciosas 
son aquellas cuyo nombre termina de alguna de estas formas (utilizar esta constante):
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

-}

pdepCilina :: Medicamento
pdepCilina = aplicarHierbas (map hierbaVerde sufijosInfecciosas)

sufijosInfecciosas :: [String]
sufijosInfecciosas = [ "sis", "itis", "emia", "cocos"]

{-
Experimento: Los laboratorios antes de publicar un medicamento, lo prueban con distintos ratones para evaluar los resultados:
Hacer la función que encuentra la cantidadIdeal. Recibe una condición y dice cuál es el primer número natural que la cumple.
> cantidadIdeal even           > cantidadIdeal (>5)
2                              6
-}

cantidadIdeal :: (Number->Bool)->Number
cantidadIdeal condicion = head (filter condicion [1..])

{-
Saber si un medicamento lograEstabilizar una comunidad de ratones. Esto sucede cuando, luego de aplicarle el medicamento a todos los ratones 
de la comunidad, se elimina el sobrepeso y todos tienen menos de 3 enfermedades. Un ratón tiene sobrepeso si pesa más de 1kg.
-}

lograEstabilizar :: Medicamento->[Raton]->Bool
lograEstabilizar medicamento = (all (==True)).(map estaEstable).(map medicamento)

estaEstable :: Raton -> Bool
estaEstable raton = pesoEstable raton && tieneMenosDeNEnfermedades 3 raton

pesoEstable :: Raton -> Bool
pesoEstable = (<1).peso

tieneMenosDeNEnfermedades :: Number -> Raton -> Bool
tieneMenosDeNEnfermedades cantidad = (<cantidad).length.enfermedades
{-
Diseñar el siguiente experimento: dado una comunidad de ratones, encontrar la potencia ideal del reduceFatFast necesaria para estabilizar la 
comunidad.
-}

potenciaIdeal :: [Raton]->Number
potenciaIdeal  = cantidadIdeal.esPotenciaIdeal

esPotenciaIdeal :: [Raton]->Number->Bool
esPotenciaIdeal ratones numero = lograEstabilizar (reduceFatFast numero) ratones


-- gucci