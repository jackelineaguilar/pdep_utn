prioridadPino :: Int -> String
prioridadPino altura	|altura == 2 ="Alta"
			|altura <= 3 && pesoPino altura> 800 = "Media"
			|sirvePino altura = "Baja"
			|otherwise = "Obsoleto"

sirvePino  = esPesoUtil.pesoPino

esPesoUtil peso =   peso >= 400 && peso <= 1000

pesoPino :: Int -> Int
pesoPino alturaPino	| alturaPino <= 3 = (alturaPino * 100 )* 3
			| otherwise  = (alturaPino * 100 ) * 2

	
------

puedoAvanzar :: String -> Bool
puedoAvanzar color	| color == "verde" = True
			| otherwise      = False 
-----
numMax x y | x > y     = x
        | otherwise = y
-----
nombreEsPar:: String -> Bool
nombreEsPar = even.length

-----
aproboAlumno :: Int -> Bool
aproboAlumno n = n >=6

-------

pesosADolares ::Float ->Float
pesosADolares pesos = pesos/40.50

-------
doble :: Int -> Int
doble n = n * 2