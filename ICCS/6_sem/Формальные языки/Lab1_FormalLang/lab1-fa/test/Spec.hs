{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Test.QuickCheck
import Test.QuickCheck.All
import Lib
import Data.List (isInfixOf)
import Data.Char (isAlpha)
import Control.Monad (liftM)

genNonLatinInput :: Gen String
genNonLatinInput = suchThat arbitrary (\s -> any isCyrillic (stripComments s))
  where
    isCyrillic c = isAlpha c && not (isLatin c)
    isLatin c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

prop_nonLatin :: Property
prop_nonLatin = forAll genNonLatinInput $ \s ->
  let (_, errors) = lexer s
  in "Упс! Пишите латинскими буквами!" `elem` errors

prop_operatorLE :: Bool
prop_operatorLE =
  let (tokens, errs) = lexer "a <= 5;"
  in tokens == [TIdentifier "a", TOp "<=", TReal 5.0, TSemicolon] && null errs

prop_operatorGE :: Bool
prop_operatorGE =
  let (tokens, errs) = lexer "b >= 10;"
  in tokens == [TIdentifier "b", TOp ">=", TReal 10.0, TSemicolon] && null errs

prop_whileCycle :: Bool
prop_whileCycle =
  case executeProgram "while a < 5 do; a := a + 1;" of
    Right res -> "a = 5" `isInfixOf` res
    _         -> False

prop_generateProgram :: Property
prop_generateProgram = ioProperty $ do
  prog <- generateProgram
  let (_, errs) = lexer prog
  return (null errs)

return []
runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = do
  putStrLn "Пошло поехало..."
  let nTestov = stdArgs {maxSuccess = 1000}
  putStrLn "Тестики для генерации цикла"
  quickCheckWith nTestov prop_generateProgram
  putStrLn "Тестики для операторов"
  quickCheckWith nTestov prop_operatorLE
  quickCheckWith nTestov prop_operatorGE
  putStrLn "Тестики для циклов"
  quickCheckWith nTestov prop_whileCycle
  