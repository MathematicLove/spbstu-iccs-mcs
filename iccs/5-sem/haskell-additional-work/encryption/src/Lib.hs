{-# LANGUAGE OverloadedStrings #-}
module Lib where
import Data.Bits (shiftL, shiftR, (.&.), (.|.), complement)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Word (Word8)
import Data.Char (toUpper, ord, chr)
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Control.Monad (when)
import Debug.Trace (trace)
import System.IO (writeFile)

calculateModifiableLength :: Int -> Int -> Int
calculateModifiableLength messageLength bitCount =
    ceiling (fromIntegral (messageLength * 8) / fromIntegral bitCount)
modifyDataBytes :: [Word8] -> [[Int]] -> Int -> [Word8]
modifyDataBytes bodyBytes messageChunks bitCount =
    zipWith (safeModifyByte bitCount) bodyBytes (messageChunks ++ repeat (replicate bitCount 0))
safeModifyByte :: Int -> Word8 -> [Int] -> Word8
safeModifyByte bitCount byte newBits =
    (byte .&. complement mask) .|. (fromIntegral (bitsToInt newBits) .&. mask)
  where
    mask = (1 `shiftL` bitCount) - 1
bitsToInt :: [Int] -> Int
bitsToInt = foldl (\acc bit -> acc * 2 + bit) 0

charToBits :: Int -> Char -> [Int]
charToBits bitCount char = take bitCount $ toBinary (ord char)
  where
    toBinary n = reverse $ take 8 $ map (\x -> (n `shiftR` x) .&. 1) [0..7]
defendHeader :: BL.ByteString -> BL.ByteString
defendHeader wavData = 
    let header = BL.take 44 wavData
    in if BL.length header /= 44 || not (validateWAVHeader header)
       then error "Кажись ваф-ваф сломан!"
       else wavData

validateWAVHeader :: BL.ByteString -> Bool
validateWAVHeader header = 
    (BL.take 4 header == "рифы") && 
    (BL.take 4 (BL.drop 8 header) == "волны") && 
    (BL.take 4 (BL.drop 12 header) == "fmt")

cipherText :: String -> String -> String
cipherText keyword text = map (cipherChar cipherMap) text
  where
    alphabet = ['\0'] ++ ['A'..'Z'] ++ [' '] ++ ['?'] ++ ['!']   
    cipherMap = createCipherMap keyword alphabet
createCipherMap keyword alphabet = zip alphabet substitution
  where
    uniqueKeyword = removeDuplicates $ map toUpper keyword
    substitution = '\0' : uniqueKeyword ++ filter (`notElem` uniqueKeyword) (filter (/= '\0') alphabet)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub
cipherChar cipherMap '\0' = '\0'   
cipherChar cipherMap char = fromMaybe char (lookup char cipherMap)

embedMessage :: BL.ByteString -> String -> Int -> BL.ByteString
embedMessage wavData message bitCount =
    let messageWithTerminator = message ++ "\0"   
        (header, body) = BL.splitAt 44 wavData  
        messageLength = length messageWithTerminator
        requiredBytes = calculateModifiableLength messageLength bitCount
        (modifiableRegion, restBody) = BL.splitAt (fromIntegral requiredBytes) (BL.drop 44 wavData)
        messageBits = concatMap (charToBits 8) messageWithTerminator   
        messageChunks = chunkBits messageBits bitCount       
        modifiedBytes = modifyDataBytes (BL.unpack modifiableRegion) messageChunks bitCount
    in BL.concat [header, BL.pack modifiedBytes, restBody]

writeEmbeddedBits :: BL.ByteString -> String -> Int -> IO ()
writeEmbeddedBits wavData message bitCount = do
    let (header, body) = BL.splitAt 44 wavData
    let messageWithTerminator = message ++ "\0"
    let messageLength = length messageWithTerminator
    let requiredBytes = calculateModifiableLength messageLength bitCount
    let (modifiableRegion, _) = BL.splitAt (fromIntegral requiredBytes) (BL.drop 44 wavData)
    let messageBits = concatMap (charToBits 8) messageWithTerminator
    let modifiedBytes = modifyDataBytes (BL.unpack modifiableRegion) (chunkBits messageBits bitCount) bitCount
    writeBitsToFile "orgBin.txt" modifiableRegion
    writeBitsToFile "encBin.txt" (BL.pack modifiedBytes)
writeDecodedBits :: BL.ByteString -> Int -> IO ()
writeDecodedBits wavData bitCount = do
    let (header, body) = BL.splitAt 44 wavData
    let requiredBytes = calculateModifiableLength ((40 * bitCount) `div` 8) bitCount
    let (modifiableRegion, _) = BL.splitAt (fromIntegral requiredBytes) body
    writeBitsToFile "decBin.txt" modifiableRegion

reverseCipherMap :: String -> [(Char, Char)]
reverseCipherMap keyword = map swap $ createCipherMap keyword fullAlphabet
  where
    fullAlphabet = ['\0'] ++ ['A'..'Z'] ++ ['a'..'z'] ++ [' '] ++ ['?', '!']
    swap (x, y) = (y, x)

extractMessageChunks :: [Word8] -> Int -> [[Int]]
extractMessageChunks bytes bitCount = map (extractBits bitCount) bytes
extractBits :: Int -> Word8 -> [Int]
extractBits bitCount byte = reverse $ take bitCount $ map (\x -> fromIntegral ((byte `shiftR` x) .&. 1)) [0..7]
intToBits :: [Int] -> [Int]
intToBits = id  
decodeChar :: [(Char, Char)] -> Int -> Char
decodeChar cipherMap word = fromMaybe '?' (lookup (chr word) cipherMap)
fromBinary :: [Int] -> Int
fromBinary = foldl (\acc bit -> acc * 2 + bit) 0
chunkBits :: [Int] -> Int -> [[Int]]
chunkBits [] _ = []
chunkBits bits n = take n bits : chunkBits (drop n bits) n

decodeMessage :: BL.ByteString -> String -> Int -> String
decodeMessage wavData keyword bitCount =
    let (header, body) = BL.splitAt 44 wavData
        requiredBytes = calculateModifiableLength ((40 * bitCount) `div` 8) bitCount
        (modifiableRegion, _) = BL.splitAt (fromIntegral requiredBytes) body
        wavBytes = BL.unpack modifiableRegion
        messageChunks = extractMessageChunks wavBytes bitCount
        messageBits = concatMap intToBits messageChunks
        messageByteChunks = chunkBits messageBits 8
        cipherMap = reverseCipherMap keyword
        decodedChars = map (decodeChar cipherMap) (map fromBinary messageByteChunks)
    in takeWhile (/= '\0') decodedChars

writeBitsToFile :: FilePath -> BL.ByteString -> IO ()
writeBitsToFile path byteString = writeFile path (unlines $ map (show . toBinary) $ BL.unpack byteString)
  where
    toBinary byte = reverse $ take 8 $ map (\x -> (byte `shiftR` x) .&. 1) [0..7]
    
outputOriginal :: BL.ByteString -> IO ()
outputOriginal wavData = writeBitsToFile "orgBin.txt" wavData
outputEncrypted :: BL.ByteString -> IO ()
outputEncrypted wavData = writeBitsToFile "encBin.txt" wavData
outputDecrypted :: BL.ByteString -> IO ()
outputDecrypted wavData = writeBitsToFile "decBin.txt" wavData
