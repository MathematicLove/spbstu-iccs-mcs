module Main where

import Lib
import System.IO (hFlush, stdout)

main :: IO ()
main = putStrLn "" >> menuLoop

menuLoop :: IO ()
menuLoop = do
  putStrLn
    "\nМеню:\n \
    \ 1  – Сгенерировать (Plusquamperfekt)🇩🇪\n \
    \ 2  – Проверить своё (Plusquamperfekt)🇩🇪\n \
    \ 3  – Сгенерировать (Präsens)🇩🇪\n \
    \ 4  – Проверить своё (Präsens)🇩🇪\n \
    \ 5  – Сгенерировать (Modal + Infinitiv)🇩🇪\n \
    \ 6  – Проверить своё (Modal + Infinitiv)🇩🇪\n \
    \ 7  – Сгенерировать (Азербайджанский: ADV+OBJ Present)🇦🇿\n \
    \ 8  – Проверить своё (Азербайджанский: ADV+OBJ Present)🇦🇿\n \
    \ 0  – Выход"
  putStr "> " >> hFlush stdout
  choice <- getLine
  case choice of
    "1" -> genAndShow deriveRandomPluperfect  >> menuLoop
    "2" -> askAndCheck "Plusquamperfekt"   checkPluperfect >> menuLoop
    "3" -> genAndShow deriveRandomPresent     >> menuLoop
    "4" -> askAndCheck "Präsens"          checkPresent     >> menuLoop
    "5" -> genAndShow deriveRandomModal       >> menuLoop
    "6" -> askAndCheck "Modal + Infinitiv" checkModal      >> menuLoop
    "7" -> genAndShow deriveRandomAzeri       >> menuLoop
    "8" -> askAndCheck "Азербайджанский"  checkAzeri      >> menuLoop
    "0" -> putStrLn "Sagolun / Auf Wiedersehen!"
    ""  -> pure ()
    _   -> putStrLn "Выберите пункт меню!" >> menuLoop
 where
  genAndShow g = g >>= putStrLn . ("Сгенерировано: " ++)

  askAndCheck tag checker = do
    putStrLn $ "Введите предложение (" ++ tag ++ "):"
    putStr " >> " >> hFlush stdout
    inp <- getLine
    case checker inp of
      OK                -> putStrLn "✅ Всё верно!"
      ENotInAlphabet ws -> putStrLn $ "❌ Есть слово вне алфавита: " ++ unwords ws
      ENotInGrammar     -> putStrLn   "❌ Не принадлежит грамматике"
