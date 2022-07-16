factorial::Float -> Float
factorial n = product [1..n]
 

neper :: Float -> Float
neper 0.0 = 1.0
neper n =  1.0 / factorial n + neper (n - 1)



euler 0.0 = 1.0
euler n = 1 / product [1..n] + euler (n-1)


fun2 :: [String]->[String]
fun2 xs = map reverse xs


sumaL:: [Float] -> Float
sumaL[] = 0
sumaL(x:xs) = x + sumaL xs


prom xs = sumaL xs / fromIntegral(length xs)

promList :: [[Float]] -> [Float]
promList xs = map prom xs
