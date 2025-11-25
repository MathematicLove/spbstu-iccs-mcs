module Main where
import System.Environment (getArgs)
import System.IO (readFile, writeFile)
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when)
import Lib (cipherText, embedMessage, decodeMessage, defendHeader, calculateModifiableLength, writeBitsToFile)
import Data.Char (toUpper, ord)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["encrypt", wavPath, textPath, keyword, bitCountStr] -> do
            let bitCount = read bitCountStr :: Int
            when (bitCount < 1 || bitCount > 8) $
                error "Только от 1 до 8 что б не поломалось ("
            content <- readFile textPath
            wavData <- BL.readFile wavPath
            let _ = defendHeader wavData
            let messageLength = length content + 1  
            let requiredBytes = calculateModifiableLength messageLength bitCount
            let (_, modifiableRegion) = BL.splitAt 40 wavData
            let (modifiablePart, _) = BL.splitAt (fromIntegral requiredBytes) modifiableRegion
            writeBitsToFile "orgBin.txt" modifiablePart
            let cipheredContent = cipherText (map toUpper keyword) content
            let maxMessageLength = maximumMessageLength bitCount
            when (length cipheredContent > maxMessageLength) $
                error $ "Слишком много текста! Должно быть: " ++ show maxMessageLength ++ " символов"
            let modifiedWavData = embedMessage wavData cipheredContent bitCount
            let (_, modifiedPart) = BL.splitAt 44 modifiedWavData
            let (encryptedPart, _) = BL.splitAt (fromIntegral requiredBytes) modifiedPart
            writeBitsToFile "encBin.txt" encryptedPart
            BL.writeFile (wavPath ++ ".encrypted.wav") modifiedWavData
            putStrLn "Зашифровало!"
        ["decrypt", wavPath, keyword, bitCountStr, outputTextPath] -> do
            let bitCount = read bitCountStr :: Int
            when (bitCount < 1 || bitCount > 8) $
                error "Только от 1 до 8 что б не поломалось ("
            wavData <- BL.readFile wavPath
            let _ = defendHeader wavData
            let messageLength = (40 * bitCount) `div` 8
            let requiredBytes = calculateModifiableLength messageLength bitCount
            let (_, modifiableRegion) = BL.splitAt 44 wavData
            let (modifiablePart, _) = BL.splitAt (fromIntegral requiredBytes) modifiableRegion
            let decodedMessage = decodeMessage wavData (map toUpper keyword) bitCount
            let decodedData = BL.pack $ map fromIntegral $ take (length decodedMessage) $ map ord decodedMessage
            writeBitsToFile "decBin.txt" decodedData
            writeFile outputTextPath decodedMessage
            putStrLn "Расшифровало!"
        _ -> putStrLn "Ошибка!"

maximumMessageLength :: Int -> Int
maximumMessageLength bitCount = (40 * bitCount) `div` 8
