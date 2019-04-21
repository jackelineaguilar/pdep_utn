primo :: Int -> Bool
primo 1 = False
primo 2 = True
primo n = noHayDivisores 2 (n - 1) n

noHayDivisores minimo maximo n
	|esDivisor minimo n = False
	|minimo == maximo = True
	|otherwise = noHayDivisores (minimo + 1) maximo n

esDivisor unNumero otroNumero = mod otroNumero unNumero == 0


 
