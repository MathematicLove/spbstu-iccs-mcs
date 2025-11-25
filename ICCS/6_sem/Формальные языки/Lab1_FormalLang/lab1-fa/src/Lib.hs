module Lib
  ( Token(..)
  , lexer
  , buildAssignMap
  , tokenLexeme
  , executeProgram
  , generateProgram
  , stripComments
  ) where

import Data.Char
import System.Random (randomRIO)
import Text.Printf (printf)

data Token
  = TWhile
  | TDo
  | TKeyword String
  | TIdentifier String
  | TReal Double
  | TAssign
  | TOp String
  | TSemicolon
  | TString String
  | TComment String
  | TLambda String
  deriving (Show, Eq)

tokenLexeme :: Token -> String
tokenLexeme t = case t of
  TWhile         -> "while"
  TDo            -> "do"
  TKeyword s     -> s
  TIdentifier s  -> s
  TReal d        -> show d
  TAssign        -> ":="
  TOp op         -> op
  TSemicolon     -> ";"
  TString s      -> s
  TComment s     -> s
  TLambda s      -> s

checkNonLatinForNonComment :: [Token] -> [String]
checkNonLatinForNonComment = concatMap f
  where
    f (TComment _) = []
    f t =
      let lx = tokenLexeme t
      in if any (\c -> isAlpha c && not (isLatin c)) lx
         then ["Можно только символы ASCII!!!"]
         else []

isLatin :: Char -> Bool
isLatin c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

stripComments :: String -> String
stripComments = id

checkVariableStartsWithNumber :: [Token] -> [String]
checkVariableStartsWithNumber [] = []
checkVariableStartsWithNumber (TReal _ : TIdentifier _ : _) =
  ["Переменная не может начинатся с числа!"]
checkVariableStartsWithNumber (_ : ts) = checkVariableStartsWithNumber ts

checkMaxConsecutive :: [Token] -> [String]
checkMaxConsecutive [] = []
checkMaxConsecutive (t : ts)
  | isComment t = checkMaxConsecutive ts
  | length (tokenLexeme t) > 16 =
      ("Слишком много символов подряд: Можно до 16!") : checkMaxConsecutive ts
  | otherwise = checkMaxConsecutive ts

lexer :: String -> ([Token], [String])
lexer input =
  let (ts, es) = lexInternal input
  in if not (null es)
     then (ts, es)
     else
       let e1 = checkNonLatinForNonComment ts
           e2 = checkVariableStartsWithNumber ts
           e3 = checkMaxConsecutive ts
           e4 = checkLoopCounts ts
       in (ts, e1 ++ e2 ++ e3 ++ e4)

checkLoopCounts :: [Token] -> [String]
checkLoopCounts tokens =
  let countWhile = length (filter (== TWhile) tokens)
      countDo    = length (filter (== TDo) tokens)
  in if countWhile /= countDo
     then if countWhile > countDo
          then ["while распознано но не закрыто с do"]
          else ["do распознано но не закрыто с while"]
     else []

lexInternal :: String -> ([Token], [String])
lexInternal [] = ([], [])
lexInternal (c : cs)
  | isSpace c = lexInternal cs
  | c == '/' && take 1 cs == "/" =
      let rest = dropWhile (/= '\n') cs
          (toks, errs) = lexInternal rest
      in (TComment ("//" ++ takeWhile (/= '\n') cs) : toks, errs)
  | c == '-' && take 1 cs == "-" =
      let rest = dropWhile (/= '\n') cs
          (toks, errs) = lexInternal rest
      in (TComment ("--" ++ takeWhile (/= '\n') cs) : toks, errs)
  | c == '/' && take 1 cs == "*" =
      lexBlockComment (drop 1 cs)
  | c == ';' =
      let (toks, errs) = lexInternal cs
      in (TSemicolon : toks, errs)
  | c == ':' && take 1 cs == "=" =
      let (toks, errs) = lexInternal (drop 1 cs)
      in (TAssign : toks, errs)
  | take 3 (c : cs) == ">>=" =
      let rest = drop 3 (c : cs)
          (toks, errs) = lexInternal rest
      in (TOp ">>=" : toks, errs)
  | c == '<' && take 2 cs == "$>" =
      let rest = drop 2 cs
          (toks, errs) = lexInternal rest
      in (TOp "<$>" : toks, errs)
  | c == '<' && not (null cs) && head cs == '>' =
      let rest = tail cs
          (toks, errs) = lexInternal rest
      in (TOp "<>" : toks, errs)
  | c == '<' && not (null cs) && head cs == '=' =
      let rest = tail cs
          (toks, errs) = lexInternal rest
      in (TOp "<=" : toks, errs)
  | c == '>' && not (null cs) && head cs == '=' =
      let rest = tail cs
          (toks, errs) = lexInternal rest
      in (TOp ">=" : toks, errs)
  | c == '<' =
      let (toks, errs) = lexInternal cs
      in (TOp "<" : toks, errs)
  | c == '>' =
      let (toks, errs) = lexInternal cs
      in (TOp ">" : toks, errs)
  | c == '+' =
      let (toks, errs) = lexInternal cs
      in (TOp "+" : toks, errs)
  | c == '.' =
      let (toks, errs) = lexInternal cs
      in (TOp "." : toks, errs)
  | c == '\\' =
      let (lam, rest) = span (/= ' ') (c : cs)
          (toks, errs) = lexInternal rest
      in (TLambda lam : toks, errs)
  | c == '"' = lexString cs
  | isAlpha c = lexIdentOrKeyword (c : cs)
  | isDigit c || c == '.' = lexNumber (c : cs)
  | otherwise =
      let (toks, errs) = lexInternal cs
      in ([], ("Неожиданный символ: " ++ [c]) : errs)

lexBlockComment :: String -> ([Token], [String])
lexBlockComment s =
  let (inside, after) = breakOnEnd s
  in case after of
       Just rest ->
         let (toks, errs) = lexInternal rest
         in (TComment ("/*" ++ inside ++ "*/") : toks, errs)
       Nothing -> ([], ["Не закрыт блочный комментарий"])
  where
    breakOnEnd :: String -> (String, Maybe String)
    breakOnEnd [] = ([], Nothing)
    breakOnEnd (x : y : xs)
      | x == '*' && y == '/' = ([], Just xs)
      | otherwise =
          let (r, m) = breakOnEnd (y : xs)
          in (x : r, m)
    breakOnEnd (x : xs) =
      let (r, m) = breakOnEnd xs
      in (x : r, m)

lexIdentOrKeyword :: String -> ([Token], [String])
lexIdentOrKeyword s =
  let (word, rest) = span isAlphaNum s
      reserved = ["while", "do", "String", "Monad", "Maybe", "Just", "Nothing",
                  "IO", "Either", "Left", "Right", "fmap", "map"]
      token = if word `elem` reserved
              then case word of
                     "while" -> TWhile
                     "do"    -> TDo
                     _       -> TKeyword word
              else TIdentifier word
      consumed = drop (length word) s
      (toks, errs) = lexInternal consumed
  in (token : toks, errs)

lexNumber :: String -> ([Token], [String])
lexNumber s =
  let (numStr, remain) = span (\c -> isDigit c || c == '.') s
  in case reads numStr of
       [(val, "")] ->
         let (toks, errs) = lexInternal remain
         in (TReal val : toks, errs)
       _ ->
         let (toks, errs) = lexInternal remain
         in ([], ("Некорректное число: " ++ numStr) : errs)

lexString :: String -> ([Token], [String])
lexString s =
  let (strVal, remain) = span (/= '"') s
  in case remain of
       [] -> ([], ["Не закрыта строка: \"" ++ strVal])
       (_ : rest) ->
         let (toks, errs) = lexInternal rest
         in (TString strVal : toks, errs)

updateMap :: String -> String -> [(String, String)] -> [(String, String)]
updateMap v val [] = [(v, val)]
updateMap v val ((x, y) : xs)
  | x == v    = (v, val) : xs
  | otherwise = (x, y) : updateMap v val xs

buildAssignMap' :: [Token] -> [(String, String)] -> [(String, String)]
buildAssignMap' [] acc = acc
buildAssignMap' (TKeyword "String" : TIdentifier v : TAssign : TString s : xs) acc =
  buildAssignMap' xs (updateMap v s acc)
buildAssignMap' (TKeyword "String" : TIdentifier v : TSemicolon : xs) acc =
  buildAssignMap' xs (updateMap v "не инициализирован" acc)
buildAssignMap' (TKeyword kw : TIdentifier v : TAssign : t : xs) acc
  | kw `elem` ["Monad", "Maybe", "Just", "Nothing", "IO", "Either", "Left", "Right"] =
      buildAssignMap' xs (updateMap v (tokenLexeme t) acc)
buildAssignMap' (TKeyword kw : TIdentifier v : TSemicolon : xs) acc
  | kw `elem` ["Monad", "Maybe", "Just", "Nothing", "IO", "Either", "Left", "Right"] =
      buildAssignMap' xs (updateMap v "не инициализирован" acc)
buildAssignMap' (TIdentifier v : TAssign : TReal r : xs) acc =
  buildAssignMap' xs (updateMap v (show r) acc)
buildAssignMap' (TIdentifier v : TAssign : TIdentifier w : xs) acc =
  buildAssignMap' xs (updateMap v w acc)
buildAssignMap' (TIdentifier v : TAssign : TString s : xs) acc =
  buildAssignMap' xs (updateMap v s acc)
buildAssignMap' (TIdentifier v : TAssign : TLambda lam : xs) acc =
  buildAssignMap' xs (updateMap v lam acc)
buildAssignMap' (TIdentifier v : TSemicolon : xs) acc =
  buildAssignMap' xs (updateMap v "не инициализирован" acc)
buildAssignMap' (_ : xs) acc = buildAssignMap' xs acc

buildAssignMap :: [Token] -> [(String, String)]
buildAssignMap ts = buildAssignMap' ts []

executeProgram :: String -> Either String String
executeProgram s =
  let (ts, es) = lexer s
  in if not (null es)
     then Left (unlines es)
     else
       let noComm     = filter (not . isComment) ts
           countWhile = length (filter (== TWhile) noComm)
           countDo    = length (filter (== TDo) noComm)
       in if countWhile /= countDo
          then if countWhile > countDo
               then Left "while распознано но не закрыто с do"
               else Left "do распознано но не закрыто с while"
          else case noComm of
                 [ TWhile, TIdentifier i1, TOp "<", TReal lim, TDo, TSemicolon,
                   TIdentifier i2, TAssign, TIdentifier i3, TOp "+", TReal st, TSemicolon]
                   | i1 == i2 && i2 == i3 ->
                       case execWhileLoop 0 lim st 0 of
                         Left e  -> Left e
                         Right r -> Right ("Цикл while: " ++ i1 ++ " = " ++ show r)
                 [ TDo, TIdentifier i1, TAssign, TIdentifier i2, TOp "+", TReal st, TSemicolon,
                   TWhile, TIdentifier i3, TOp "<", TReal lim, TSemicolon]
                   | i1 == i2 && i2 == i3 ->
                       case execDoWhileLoop 0 lim st 0 of
                         Left e  -> Left e
                         Right r -> Right ("Цикл do while: " ++ i1 ++ " = " ++ show r)
                 _ -> Left "Неподдерживаемый формат программы"
  where
    isComment (TComment _) = True
    isComment _            = False

execWhileLoop :: Double -> Double -> Double -> Int -> Either String Double
execWhileLoop cur lim st i
  | i > 1000  = Left "Бесконечный цикл (while?)"
  | cur < lim = execWhileLoop (cur + st) lim st (i + 1)
  | otherwise = Right cur

execDoWhileLoop :: Double -> Double -> Double -> Int -> Either String Double
execDoWhileLoop cur lim st i
  | i > 1000  = Left "Бесконечный цикл (do while?)"
  | otherwise =
      let newVal = cur + st
      in if newVal < lim
         then execDoWhileLoop newVal lim st (i + 1)
         else Right newVal

generateProgram :: IO String
generateProgram = do
  mode <- randomRIO (0 :: Int, 1)
  let vars = ["i", "x", "count"]
  idx <- randomRIO (0, length vars - 1)
  let var = vars !! idx
  lim <- randomRIO (5 :: Double, 15 :: Double)
  st  <- randomRIO (1 :: Double, 3 :: Double)
  let limStr = printf "%.2f" lim
      stStr  = printf "%.2f" st
  if mode == 0
    then return . unlines $
      [ "-- Генерация while"
      , "while " ++ var ++ " < " ++ limStr ++ " do;"
      , "  " ++ var ++ " := " ++ var ++ " + " ++ stStr ++ ";"
      ]
    else return . unlines $
      [ "-- Генерация do while"
      , "do " ++ var ++ " := " ++ var ++ " + " ++ stStr ++ ";"
      , "  while " ++ var ++ " < " ++ limStr ++ ";"
      ]
      
isComment :: Token -> Bool
isComment (TComment _) = True
isComment _            = False
