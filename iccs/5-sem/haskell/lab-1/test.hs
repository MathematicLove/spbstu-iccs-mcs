
import Data.List
import Data.Fixed (mod')
import Data.Ord

--1 пять глав-- 
kernel :: Float -> Float
factorial :: Integer -> Integer
ecz :: Integer -> Integer
sumf :: Integer -> Integer
fibonachi :: Integer -> Integer
kernel 0 = 1
kernel n = 2*(n * n)
ecz 0 = 1
ecz m = (m+1*2) * ecz (m-1)
factorial 0 = 1
factorial n = n * factorial (n - 1)
sumf 0 = 0
sumf n = n + sumf (n - 1)
fibonachi 0 = 0
fibonachi n = n + fibonachi n - 1 
main :: IO ()
main = do
    putStrLn "Введите число для суммы:"
    sumInput <- getLine
    let sumNumber = read sumInput :: Integer
    print (sumf sumNumber)
    putStrLn "Введите число для факториала:"
    factInput <- getLine
    print("Enter nums for kernel & fibonachi")
    kerInput <- getLine
    fiboInput <- getLine
    let factNumber = read factInput :: Integer
    let kerNum = read kerInput :: Float
    let fiboNum = read fiboInput :: Integer
    print (factorial factNumber)
    print (ecz factNumber)
    print (kernel kerNum)
    print (fibonachi fiboNum)
    putStrLn "Как тебя зовут:"
    uname <- getLine
    print("Your name is: " ++ uname)


