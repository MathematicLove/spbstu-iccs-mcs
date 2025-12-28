module Lib
  ( deriveRandomPluperfect , deriveRandomPresent , deriveRandomModal , deriveRandomAzeri
  , checkPluperfect        , checkPresent        , checkModal        , checkAzeri
  , Check(..)
  ) where

import Data.Char     (isUpper, toUpper, toLower)
import Data.List     (nub)
import System.Random (randomRIO)

data Check = OK | ENotInAlphabet [String] | ENotInGrammar
  deriving (Eq, Show)

rand :: [a] -> IO a
rand xs = (xs !!) <$> randomRIO (0, length xs - 1)

randMaybe :: [a] -> IO (Maybe a)
randMaybe xs = do
  bit <- randomRIO (0 :: Int, 1)
  if bit == 1 then Just <$> rand xs else pure Nothing

capitalize :: String -> String
capitalize (c:cs) = toUpper c : cs
capitalize []     = []

mkSentence :: [String] -> IO String
mkSentence (w:ws) = do
  punctuation <- rand [".", "!", "?"]
  pure $ capitalize w ++ concatMap render ws ++ punctuation
 where
  render "," = ","            
  render x   = ' ' : x         
mkSentence [] = error "empty sentence"


tokenize :: String -> Maybe [String]     
tokenize s
  | null s || not (last s `elem` ['.', '!', '?']) = Nothing
  | otherwise               = Just (reverse (go (init s) [] ""))
 where
  go [] acc cur        = finish cur acc
  go (',' : xs) acc cur = go xs ("," : finish cur acc) ""
  go (' ' : xs) acc cur = go xs (finish cur acc) ""
  go (c   : xs) acc cur = go xs acc (cur ++ [c])

  finish "" a = a
  finish w  a = w:a

pronouns :: [String]
pronouns = ["ich","du","er","sie","es","wir","ihr","sie_fml"]

displayPron :: String -> String
displayPron "sie_fml" = "Sie"
displayPron p         = capitalize p

personIx :: String -> Int
personIx p = case p of
  "ich"      -> 0
  "du"       -> 1
  "er"       -> 2 ; "sie" -> 2 ; "es" -> 2
  "wir"      -> 3
  "ihr"      -> 4
  "sie_fml"  -> 5
  _          -> error "unknown pronoun"

advDE, ppartsDE, detsAcc, nounsAcc :: [String]
advDE    = ["heute","gestern","schon","oft","morgen"]
ppartsDE = ["gesehen","gegessen","gemacht","gegangen","geschlafen","geschrieben","gesagt"]
detsAcc  = ["einen","eine","ein","den","die","das"]
nounsAcc = ["Brief","Apfel","Hund","Katze","Buch","Auto","Haus","Zeitung"]

habenF, seinF :: [String]
habenF = ["hatte","hattest","hatte","hatten","hattet","hatten"]
seinF  = ["war"  ,"warst"  ,"war" ,"waren" ,"wart"  ,"waren"]

auxPlu :: [String]
auxPlu = habenF ++ seinF

alphaPlu :: [String]
alphaPlu = nub (map displayPron pronouns ++ auxPlu ++ advDE
                ++ detsAcc ++ nounsAcc ++ ppartsDE ++ [","])

deriveRandomPluperfect :: IO String
deriveRandomPluperfect = do
  subjI <- rand pronouns
  let idx = personIx subjI
  useHaben <- rand [True, False]
  let aux = if useHaben then habenF !! idx else seinF !! idx
  advM <- randMaybe advDE
  objM <- randMaybe $ (,) <$> detsAcc <*> nounsAcc
  pp   <- rand ppartsDE
  let core = case (advM, objM) of 
        (Nothing, Nothing)        -> [displayPron subjI, aux, pp]
        (Just a , Nothing)        -> [displayPron subjI, aux, a, pp]
        (Nothing, Just (d,n))     -> [displayPron subjI, aux, d, n, pp]
        (Just a , Just (d,n))     -> [displayPron subjI, aux, a, ",", d, n, pp]
  mkSentence core

newtype GVerb = GVerb { formsG :: [String] }

verbsPräs :: [GVerb]
verbsPräs =
  map GVerb
    [ ["sehe","siehst","sieht","sehen","seht","sehen"]
    , ["esse","isst","isst","essen","esst","essen"]
    , ["mache","machst","macht","machen","macht","machen"]
    , ["gehe","gehst","geht","gehen","geht","gehen"]
    , ["schlafe","schläfst","schläft","schlafen","schlaft","schlafen"]
    ]

objectsDE :: [String]
objectsDE = ["den_Apfel","das_Buch","die_Katze","den_Hund","mich","dich","ihn","sie","es","nichts"]

alphaPräs :: [String]
alphaPräs = nub (alphaPlu ++ concatMap formsG verbsPräs ++ objectsDE)

deriveRandomPresent :: IO String
deriveRandomPresent = do
  subjI <- rand pronouns
  GVerb fs <- rand verbsPräs
  let verb = fs !! personIx subjI
  advM <- randMaybe advDE
  obj  <- rand objectsDE
  let core = case advM of
        Nothing -> [displayPron subjI, verb, obj]
        Just a  -> [displayPron subjI, verb, a, ",", obj]
  mkSentence core

newtype MVerb = MVerb { formsM :: [String] }

modals :: [MVerb]
modals =
  map MVerb
    [ ["will","willst","will","wollen","wollt","wollen"]
    , ["kann","kannst","kann","können","könnt","können"]
    , ["muss","musst","muss","müssen","müsst","müssen"]
    ]

infinitives :: [String]
infinitives = ["sehen","essen","machen","gehen","schlafen","schreiben","sagen"]

alphaModal :: [String]
alphaModal = nub (alphaPräs ++ concatMap formsM modals ++ infinitives)

deriveRandomModal :: IO String
deriveRandomModal = do
  subjI <- rand pronouns
  MVerb fs <- rand modals
  let modal = fs !! personIx subjI
  adv  <- rand advDE
  objM <- randMaybe objectsDE
  inf  <- rand infinitives
  let core = case objM of
        Nothing -> [displayPron subjI, modal, adv, ",", inf]
        Just o  -> [displayPron subjI, modal, adv, o, ",", inf]
  mkSentence core

pronAZ, advAZ, objAZ :: [String]
pronAZ = ["men","sen","o","biz","siz","onlar"]
advAZ  = ["bugun","dunen","tez-tez","hemishe"]
objAZ  = ["kitabi","evi","alma","mashini","mektubu"]

newtype AzVerb = AzVerb { formsAz :: [String] }

azVerbs :: [AzVerb]
azVerbs =
  map AzVerb
    [ ["oxuyuram","oxuyursan","oxuyur","oxuyuruq","oxuyursunuz","oxuyurlar"]
    , ["yazıram","yazırsan","yazır","yazırıq","yazırsınız","yazırlar"]
    , ["gedirəm","gedirsən","gedir","gedirik","gedirsiniz","gedirlər"]
    , ["görürəm","görürsən","görür","görürük","görürsünüz","görürlər"]
    , ["alıram","alırsan","alır","alırıq","alırsınız","alırlar"]
    , ["sevirəm","sevirsən","sevir","sevirik","sevirsiniz","sevirlər"]
    ]

personIxAZ :: String -> Int
personIxAZ p = case p of
  "men"   -> 0; "sen" -> 1; "o" -> 2
  "biz"   -> 3; "siz" -> 4; "onlar" -> 5
  _       -> error "unknown"

alphaAZ :: [String]
alphaAZ = nub (map capitalize pronAZ ++ advAZ ++ objAZ
               ++ concatMap formsAz azVerbs ++ [","])

deriveRandomAzeri :: IO String
deriveRandomAzeri = do
  subj <- rand pronAZ
  adv  <- rand advAZ
  obj  <- rand objAZ
  AzVerb fs <- rand azVerbs
  let verb = fs !! personIxAZ subj
  mkSentence [subj, adv, obj, ",", verb]

checkWith :: ([String] -> Check) -> [String] -> String -> Check
checkWith validator alpha s =
  case tokenize s of
    Nothing   -> ENotInGrammar
    Just toks ->
      let bad = filter (`notElem` alpha) toks
      in  if not (null bad) then ENotInAlphabet bad
          else validator toks

checkPluperfect :: String -> Check
checkPluperfect = checkWith go alphaPlu
 where
  go (subTok:auxTok:rest)
    | isUpper (head subTok)
    , auxTok `elem` auxPlu
    , let (mid, [pp]) = splitAt (length rest - 1) rest
    , pp `elem` ppartsDE
    , validMid mid
    = OK
  go _ = ENotInGrammar

  validMid []                          = True
  validMid [adv]                       = adv `elem` advDE 
  validMid [det,noun]                  = det `elem` detsAcc && noun `elem` nounsAcc
  validMid [adv,",",det,noun]          = adv `elem` advDE
                                       && det `elem` detsAcc && noun `elem` nounsAcc
  validMid _                           = False

checkPresent :: String -> Check
checkPresent = checkWith go alphaPräs
 where
  go (subTok:verbTok:rest)
    | isUpper (head subTok)
    , let idx = personIx (fixSie (map toLower subTok))
          fixSie "sie" = "sie_fml"
          fixSie x     = x
    , any (\(GVerb fs) -> fs !! idx == verbTok) verbsPräs
    , validTail rest
    = OK
  go _ = ENotInGrammar

  validTail [obj]                 = obj `elem` objectsDE
  validTail [adv,",",obj]         = adv `elem` advDE && obj `elem` objectsDE
  validTail _                     = False

checkModal :: String -> Check
checkModal = checkWith go alphaModal
 where
  go (subTok:modalTok:advTok:rest)
    | isUpper (head subTok)
    , advTok `elem` advDE
    , let idx = personIx (fixSie (map toLower subTok))
          fixSie "sie" = "sie_fml"
          fixSie x     = x
    , any (\(MVerb fs) -> fs !! idx == modalTok) modals
    , valid rest
    = OK
  go _ = ENotInGrammar

  valid [",",inf]             = inf `elem` infinitives
  valid [obj,",",inf]         = obj `elem` objectsDE && inf `elem` infinitives
  valid _                     = False

checkAzeri :: String -> Check
checkAzeri = checkWith go alphaAZ
 where
  go (subTok:advTok:objTok:",":verbTok:[])
    | isUpper (head subTok)
    , let idx = personIxAZ (map toLower subTok)
    , advTok `elem` advAZ
    , objTok `elem` objAZ
    , any (\(AzVerb fs) -> fs !! idx == verbTok) azVerbs
    = OK
  go _ = ENotInGrammar