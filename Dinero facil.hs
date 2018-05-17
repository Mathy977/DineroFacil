--https://docs.google.com/document/d/1WDH1gFiQoy1hqafe9Vvt-o_GTnHsCEFse9xa2Aafc84/edit
--Dinero Facil
cacho = UnaPersona "pedro" [(100, "peso"), (400, "real"), (1, "dolar")] 4
tita = UnaPersona "juana" [(1000, "euro"), ((-5), "peso")] 2

data Producto = UnProducto {descripcion::String, precio::Float, moneda::String}

cotizaciones :: [(Float,String)]
cotizaciones = [(1, "peso"), (9, "dolar"), (4, "real"), (8,"euro")]

--data persona
data Persona = UnaPersona {nombre::String, listaAhorro::[(Int,String)], nivelDeSatifaccion::Int}

--1)Para entrar en calor
--a)Obtener el tipo de cambio de una moneda

tipoCambio tipo = fst $ head $ filter ((== tipo).snd) cotizaciones

--b)Convertir un cierto importe, de una moneda a otra
--convertirA tipoAConvertir (cant,tipo) = (cant * (tipoCambio tipo))/(tipoCambio tipoAConvertir)

 --c)Obtener la cantidad de dinero que tiene una persona en una moneda dada
ahorrrosEn tipo lista = fst $ head $ filter ((== tipo).snd) lista
cantidadDe tipo persona = ahorrrosEn tipo (listaAhorro persona)

--d) Calcular el total de ahorro de una persona, expresado en pesos.
totalAhorro persona = sum( map (\x->ahorrrosEn)) 