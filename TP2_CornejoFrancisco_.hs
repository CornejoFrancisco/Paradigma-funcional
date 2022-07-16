
{- 1) crecimientoPorAnio  -}
crecimientoPorAnio::(Float)->String
crecimientoPorAnio n|  n == 1 = "Tine que crecer 22 cm" 	            
		    |  n == 2  = "Tiene que crecer 20 cm" 		    
 	            |  n ==  3 = "Tiene que crecer 18 cm"  		    
		    |  n ==  4  = "Tiene que crecer 16 cm" 		   
		    |  n ==  5 = "Tiene que crecer 14 cm"  		
		    |  n ==  6  = "Tiene que crecer 12 cm" 		
		    |  n ==  7 = "Tiene que crecer 10 cm"   		
		    |  n ==  8  = "Tiene que crecer 8 cm" 		
	            |  n ==  9 = "Tiene que crecer 6 cm"   		
		    |  n ==  10 = "Tiene que crecer 4 cm"  		
		    |  n ==  11  = "Tiene que crecer 2 cm" 		
		    |  n ==  12 = "Tiene que crecer 1 cm"
		    |  n >=  13 = "Tiene que crecer 0 cm"	
	    
{- Equivalencia en crecimientos -}
crecimiento::Integer->Integer
crecimiento n|  n == 1 = 22	            
	     |  n == 2  = 20  		    
 	     |  n ==  3 = 18 	    
	     |  n ==  4  = 16	   
	     |  n ==  5 = 14
       	     |  n ==  6  = 12 		
	     |  n ==  7 = 10 
	     |  n ==  8  = 8	    
             |  n ==  9 =  6		
	     |  n ==  10 =  4 		
             |  n ==  11  = 2 		 
       	     |  n ==  12 =  1  
             |  n >=  13 =  0  

{- Calculo para calcular el mayor. -}
crecimientoMayor:: Integer -> Integer  
crecimientoMayor  0 = 0
crecimientoMayor  n = crecimiento n + crecimientoMayor (n - 1 )

{- Calculo para calcular el menor. -}
crecimientoMenor:: Integer -> Integer 
crecimientoMenor  0 = 0
crecimientoMenor  n = crecimientoMayor (n - 1) 


{- 2) crecimientoEntre -}
crecimientoEntreEdades:: Integer -> Integer -> Integer
crecimientoEntreEdades desde hasta  = crecimientoMayor hasta - crecimientoMenor desde



{- 3) alturasEnUnAnio -}
alturasEnUnAnio :: Integer -> [Integer]-> [Integer]
alturasEnUnanio  [] = []
alturasEnUnAnio a lista = [crecimiento a + x | x <-lista ]


{- 4)  alturaEnEdades -}
alturaEnEdades :: Integer -> Integer-> [Integer] -> [Integer]
alturaEnEdades altura edad lista = [crecimientoEntreEdades edad x  + altura  | x <-lista ]


{- Calculo auxiliar -}
altura :: Integer -> Integer-> [Integer] -> [Integer]
altura cm edad lista = [crecimientoEntreEdades edad (edad + x) + cm | x <-lista]


{- 5) alturasPara -}
alturasPara :: [(Integer, Integer)] -> [Integer] -> [[Integer]]
alturasPara [(edad, cm)][lista] = [altura cm edad [lista]]
alturasPara (x:xs) (anio: anios) = [altura  (snd x) (fst x) [anio]] ++ alturasPara xs anios





                               
