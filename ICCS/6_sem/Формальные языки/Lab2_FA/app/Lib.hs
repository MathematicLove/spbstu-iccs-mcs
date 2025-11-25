module Lib
    ( checkYandexDiskLink
    , generateValidLink
    ) where

import Data.Char (isAscii, isSpace, isControl)
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Data.List (isInfixOf)

validChars :: String
validChars = ['a'..'z'] ++ ['0'..'9'] ++ "-_"

preprocess :: String -> String
preprocess = filter (not . isSpace) . map toLower
  where
    toLower c =
      if c >= 'A' && c <= 'Z'
      then toEnum (fromEnum c + 32)
      else c

data State = Start 
           | H | Ht | Htt | Https 
           | HttpsColon | HttpsColonSlash | HttpsColonSlashSlash
           | D | Di | Dis | Disk 
           | DiskDot | DiskDotY | DiskDotYa | DiskDotYan | DiskDotYand
           | DiskDotYande | DiskDotYandex 
           | YandexDot | YandexDotR | YandexDotRu 
           | YandexDotRuSlash
           | YandexDotRuSlashD 
           | YandexDotRuSlashDSlash
           | YandexDotRuSlashC
           | YandexDotRuSlashCl
           | YandexDotRuSlashCli
           | YandexDotRuSlashClie
           | YandexDotRuSlashClien
           | YandexDotRuSlashClient
           | YandexDotRuSlashClients
           | YandexDotRuSlashClientsSlash
           | YandexDotRuSlashClientsSlashD
           | YandexDotRuSlashClientsSlashDi
           | YandexDotRuSlashClientsSlashDis
           | YandexDotRuSlashClientsSlashDisk
           | YandexDotRuSlashClientsSlashDiskSlash
           | Y | Ya | Yad | Yadi
           | YadiDot | YadiDotS | YadiDotSk
           | YadiDotSkSlash
           | YadiDotSkSlashD
           | YadiDotSkSlashDSlash
           | Final
           deriving (Eq, Ord, Show, Enum, Bounded)

isValidChar :: Char -> Bool
isValidChar c = not (isSpace c || isControl c)

transition :: State -> Char -> State
transition Start 'h'                = H
transition H 't'                    = Ht
transition Ht 't'                   = Htt
transition Htt 'p'                  = Https
transition Https 's'                = HttpsColon
transition HttpsColon ':'           = HttpsColonSlash
transition HttpsColonSlash '/'      = HttpsColonSlashSlash
transition HttpsColonSlashSlash 'd' = D
transition HttpsColonSlashSlash 'y' = Y
transition Start 'd' = D
transition Start 'y' = Y
transition D 'i' = Di
transition Di 's' = Dis
transition Dis 'k' = Disk
transition Disk '.' = DiskDot
transition DiskDot 'y'      = DiskDotY
transition DiskDotY 'a'     = DiskDotYa
transition DiskDotYa 'n'    = DiskDotYan
transition DiskDotYan 'd'   = DiskDotYand
transition DiskDotYand 'e'  = DiskDotYande
transition DiskDotYande 'x' = DiskDotYandex
transition DiskDotYandex '.' = YandexDot
transition YandexDot 'r'       = YandexDotR
transition YandexDotR 'u'      = YandexDotRu
transition YandexDotRu '/' = YandexDotRuSlash
transition YandexDotRuSlash 'd' = YandexDotRuSlashD
transition YandexDotRuSlash 'c' = YandexDotRuSlashC    
transition YandexDotRuSlashD ' ' = YandexDotRuSlashD
transition YandexDotRuSlashD '/' = YandexDotRuSlashDSlash
transition YandexDotRuSlashDSlash _ = Final
transition YandexDotRuSlashC 'l' = YandexDotRuSlashCl
transition YandexDotRuSlashCl 'i' = YandexDotRuSlashCli
transition YandexDotRuSlashCli 'e' = YandexDotRuSlashClie
transition YandexDotRuSlashClie 'n' = YandexDotRuSlashClien
transition YandexDotRuSlashClien 't' = YandexDotRuSlashClient
transition YandexDotRuSlashClient 's' = YandexDotRuSlashClients
transition YandexDotRuSlashClients '/' = YandexDotRuSlashClientsSlash
transition YandexDotRuSlashClientsSlash 'd' = YandexDotRuSlashClientsSlashD
transition YandexDotRuSlashClientsSlashD 'i' = YandexDotRuSlashClientsSlashDi
transition YandexDotRuSlashClientsSlashDi 's' = YandexDotRuSlashClientsSlashDis
transition YandexDotRuSlashClientsSlashDis 'k' = YandexDotRuSlashClientsSlashDisk
transition YandexDotRuSlashClientsSlashDisk '/' = YandexDotRuSlashClientsSlashDiskSlash
transition YandexDotRuSlashClientsSlashDiskSlash _ = Final
transition Y 'a'     = Ya
transition Ya 'd'    = Yad
transition Yad 'i'   = Yadi
transition Yadi '.'  = YadiDot
transition YadiDot 's' = YadiDotS
transition YadiDotS 'k' = YadiDotSk
transition YadiDotSk '/' = YadiDotSkSlash
transition YadiDotSkSlash 'd' = YadiDotSkSlashD
transition YadiDotSkSlashD '/' = YadiDotSkSlashDSlash
transition YadiDotSkSlashDSlash _ = Final

transition Final c
    | isValidChar c = Final
    | otherwise     = Start
transition s ' '
    | s /= Final  = s
    | otherwise   = Final
transition _ _ = Start

checkYandexDiskLink :: String -> IO ()
checkYandexDiskLink input = do
    if not (all isAscii input)
        then putStrLn "Упс! В строке есть не-ASCII символы."
        else do
            let processed = preprocess input
            if isMainPage processed
                then putStrLn "Основная страница Яндекс Диска"
                else do
                    let result = processString processed Start
                    if result == Final && hasValidPath processed
                        then putStrLn "Строка - ссылка Яндекс Диска"
                        else putStrLn "Упс! не ссылка Яндекс Диск"
  where
    isMainPage :: String -> Bool
    isMainPage s = 
        let normalized = preprocess s
        in  normalized == "https://disk.yandex.ru"
         || normalized == "disk.yandex.ru"
         || normalized == "https://disk.yandex.ru/"
         || normalized == "disk.yandex.ru/"
         || normalized == "disk.yandex.ru/clients/"
         || normalized == "disk.yandex.ru/clients/disk/"
         || normalized == "yadi.sk"
         || normalized == "yadi.sk/"

    processString :: String -> State -> State
    processString [] st     = st
    processString (c:cs) st = processString cs (transition st c)
    
    hasValidPath :: String -> Bool
    hasValidPath s
      | "disk.yandex.ru/d/" `isInfixOf` s = True
      | "disk.yandex.ru/clients/disk/" `isInfixOf` s = True
      | "yadi.sk/d/" `isInfixOf` s = True
      | otherwise = False

generateValidLink :: IO String
generateValidLink = do
    idLength <- randomRIO (12, 20)
    idPart   <- replicateM idLength (randomChar validChars)
    patternIndex <- randomRIO (1 :: Int, 3)
    return $ case patternIndex of
        1 -> "https://disk.yandex.ru/d/" ++ idPart
        2 -> "https://disk.yandex.ru/clients/disk/" ++ idPart
        3 -> "https://yadi.sk/d/" ++ idPart
        _ -> "https://disk.yandex.ru/d/" ++ idPart
  where
    randomChar :: String -> IO Char
    randomChar chars = do
        idx <- randomRIO (0, length chars - 1)
        return (chars !! idx)
