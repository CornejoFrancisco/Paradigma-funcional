
inserta e [] = [e]
inserta e (x:xs) | e <= x = e : (x:xs) 
		| otherwise = x: inserta e xs

inserta e [] = [e]

inserta e (x:xs) | e <= x = e : (x:xs)
		|otherwise = x : inserta e xs

puntaje p |  (p >= 90 && p <= 100)= "Nivel maximo"
	  | (p >= 75 && p <= 89)= "Nivel muy bueno"
	  | (p >= 60 && p <= 74)= "Nivel es bueno"
   	  | (p >= 45 && p <= 59) = "Nivel regular"
	  | (p >= 0 && p <= 44)= "Nivel reprobado"
	| otherwise = "Puntaje errorneo"


puntajeParametros lista inf sup = [x| x <- lista, x > inf, x < sup]


cantidadMayor [] _= 0
cantidadMayor (x:xs) p = if x >= p then 1 + cantidadMayor xs p else 
					cantidadMayor xs p 





raices :: Float-> Float-> Float-> (Float, Float)
raices a b c | discriminante<0 = error "Raices no reales"
             | otherwise = ((-b + raiz)/denominador, (-b - raiz)/denominador)
  where
    discriminante = (b*b - 4*a*c)
	raiz          = sqrt discriminante
	denominador   = 2*a