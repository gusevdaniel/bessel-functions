import System.Environment
import Data.Complex

fi(a) = fromIntegral(a)

moduleFunc :: (Double) -> Double
moduleFunc(a) = sqrt(a^2)

myFact :: (Int) -> Integer
myFact(a) | a > 1 = fi(a) * myFact(a - 1)
          | a < 0 = -1
          | otherwise = 1

-- Для асимптот
u :: (Int) -> Double
u (v) = 4 * fi(v)^2

eE :: (Int, Double) -> Double
eE (v, x) = x - (fi(v)/2 + 1/4) * pi

qQ :: (Int, Double) -> Double
qQ (v, x) = (u(v) - 1) / (8 * x) - ((u(v) - 1) * (u(v) - 9) * (u(v) - 25)) / ( fi(myFact(3)) * (8 * x)^3)

pP :: (Int, Double) -> Double
pP (v, x) = 1 - ( ((u(v) - 1) * (u(v) - 9)) / (fi(myFact(2)) * (8 * x)^2) ) + ((u(v) - 1) * (u(v) - 9) * (u(v) - 25) * (u(v) - 49)) / ( fi(myFact(4)) * (8 * x)^4)

{-
    Функция Бесселя первого рода
    k - iterations
    v - порядок функции
    sum - суммирование
-}
bessSumPositive :: (Int, Double, Int, Double) -> Double
bessSumPositive(v, x, k, sum)   | k > 22 = sum
                                | otherwise = sum + bessSumPositive(v, x, k + 1, (((-1)^k) / fi(myFact(k)*myFact(v+k)))*((x/2)^fi(v+2*k)))

bessFunctionPos :: (Int, Double) -> Double
bessFunctionPos(v, x) = bessSumPositive(v, x, 0, 0)

-- v - целое или 0
bessSumNegative :: (Int, Double, Int, Double) -> Double
bessSumNegative(v, x, k, sum) = bessSumPositive(v, x, k, sum) * (-1)^fi(v)

bessFunctionNeg :: (Int, Double) -> Double
bessFunctionNeg(v, x) = bessSumNegative(v, x, 0, 0)

-- Асимптота для Бесселя
acimptBess :: (Int, Double) -> Double
acimptBess (v, x) = sqrt(2 / (pi * x)) * (pP(v, x) * cos(eE(v, x)) - qQ(v, x) * sin(eE(v, x)))

-- Итоговая функция Бесселя
bessFunction :: (Int, Double) -> Double
bessFunction(v, x) | (x < 15) && (x > (-15)) && (v >= 0) = bessFunctionPos(v, x)
                   | (x < 15) && (x > (-15)) && (v < 0)  = bessFunctionNeg((-v), x)
                   |  x >= 15                            = acimptBess(v, x)
                   |  x <= (-15)                         = acimptBess(v, moduleFunc(x))*(-1)**(moduleFunc(fi(v)))

-- Комплексный логарифм
ln :: (Double) -> Complex Double
ln(x) | x > 0 = log(x) :+ 0
      | x < 0 = log(moduleFunc(x)) :+ pi

-- ПСИ-Функция
gamma = 0.5772156649 -- Постоянная Эйлера

psiSum :: (Int, Int, Double) -> Double
psiSum(n, k, sum)   | k > (n-1) = sum
                    | otherwise = sum + psiSum(n, k + 1, 1/fi(k))

psi :: (Int) -> Double
psi(n)  | n == 1 = -gamma
        | n >= 2 = -gamma + psiSum(n, 1, 0)

-- Функция Неймана
sumOne :: (Int, Double, Int, Double) -> Double
sumOne(v, x, k, sum)    | k > (v-1) = sum
                        | otherwise = sum + sumOne(v, x, k + 1, (fi(myFact(v-k-1))/fi(myFact(k)))*((x/2)**fi(2*k)))

sumTwo :: (Int, Double, Int, Double) -> Double
sumTwo(v, x, k, sum)    | k > 22 = sum
                        | otherwise = sum + sumTwo(v, x, k + 1, (psi(k+1)+psi(v+k+1))*(((-(x^2)/4)^fi(k)) / fi(myFact(k)*myFact(v+k))))

neumannFunctionPos :: (Int, Double) -> Complex Double
neumannFunctionPos(v, x) = ((-(((x/2)**fi(-v)) / pi)*sumOne(v, x, 0, 0)) :+ 0)
                        +((2/pi) :+ 0)*ln(x/2)*(bessSumPositive(v, x, 0, 0) :+ 0)
                        -((((x/2)^fi(v))/pi)*sumTwo(v, x, 0, 0) :+ 0)                     

-- v - целое или 0
neumannFunctionNeg :: (Int, Double) -> Complex Double
neumannFunctionNeg(v, x) = neumannFunctionPos(v, x) * (-1)^fi(v)

-- Асимптота для Неймана
acimptNeuman :: (Int, Double) -> Double
acimptNeuman (v, x) = sqrt(2 / (pi * x)) * (pP(v, x) * sin(eE(v, x)) + qQ(v, x) * cos(eE(v, x)))

-- Итоговая функция Неймана
neumannFunction :: (Int, Double) -> Complex Double
neumannFunction(v, x)   | (x < 17) && (x > (-17)) && (v >= 0)   = neumannFunctionPos(v, x)
                        | (x < 17) && (x > (-17)) && (v < 0)    = neumannFunctionNeg((-v), x)
                        | x >= 17                               = acimptNeuman (v, x)               :+ 0
                        | x <= (-17)                            = (-acimptNeuman(v, moduleFunc(x))) :+ (2*sin(fi(v)*pi)*(cos(fi(v)*pi)/sin(fi(v)*pi))*bessFunction(v, moduleFunc(x)))

-- Функции Ханкеля
hankelOne :: (Int, Double) -> Complex Double
hankelOne(v, x) = (bessFunction(v, x)-imagPart(neumannFunction(v, x))) :+ realPart(neumannFunction(v, x))

hankelTwo :: (Int, Double) -> Complex Double
hankelTwo(v, x) = (bessFunction(v, x)+imagPart(neumannFunction(v, x))) :+ (-realPart(neumannFunction(v, x)))

-- Вывод справки
showHelp :: IO ()
showHelp = putStrLn "Format: function (-b, -n, -h, -hh) v x"

calculate :: [String] -> IO ()
calculate args = do
    let v :: Int
        v = read $ args!!1 :: Int

    let x :: Double
        x = read $ args!!2 :: Double

    if (args!!0 == "-b")
        then do  
            let buff = show(v) ++ " " ++ show(x) ++ " " ++ show(bessFunction(v, x))
            writeFile "result.txt" buff
        else return ()

    if (args!!0 == "-n")
        then do  
            let buff = show(v) ++ " " ++ show(x) ++ " " ++ show(neumannFunction(v, x))
            writeFile "result.txt" buff
        else return ()

    if (args!!0 == "-h")
        then do  
            let buff = show(v) ++ " " ++ show(x) ++ " " ++ show(hankelOne(v, x))
            writeFile "result.txt" buff
        else return ()

    if (args!!0 == "-hh")
        then do  
            let buff = show(v) ++ " " ++ show(x) ++ " " ++ show(hankelTwo(v, x))
            writeFile "result.txt" buff
        else return ()

    --print(bessFunction(v, x))
    --print(neumannFunction(v, x))

main = do
    args <- getArgs
    if any (\arg -> arg == "--help") args 
       then showHelp
       else calculate args