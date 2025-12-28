{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wall #-}

module Lib
  ( NonTerm(..)
  , Production(..)
  , grammar
  , firstSets, followSets, parseTable
  , printFirstSets, printFollowSets, printParseTable
  , parse                     
  , genStrings
  , saveGeneratedStringsToFile
  , loadGeneratedStrings
  , genInteract
  , setup                     
  , pause
  ) where

import           Data.List       (intercalate, tails, isPrefixOf, minimumBy)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set        as S
import           System.IO       (writeFile, readFile, hFlush, stdout)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Directory (doesFileExist)
import qualified Graphics.UI.Threepenny as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Canvas as UI
import           Graphics.UI.Threepenny.Canvas (Point(..))
import           Control.Monad   (replicateM, forM_, forM, void)
import           System.Random   (randomRIO)
import           Data.IORef      (newIORef, readIORef, writeIORef)
import           Data.Ord        (comparing)
import           Data.Maybe      (fromMaybe)
import           Control.Concurrent (threadDelay)


prompt :: String -> IO ()
prompt s = putStr s >> hFlush stdout

pause :: IO ()
pause = putStrLn "Press Enter to continue…" >> void getLine


data NonTerm = S' | S | A | B | C deriving (Eq, Ord, Show)
data Symbol  = NT NonTerm | T String | Eps deriving (Eq, Ord, Show)

data Production = P { lhs :: NonTerm, rhs :: [Symbol], act :: [String] -> String }


codeB, codeC, codeAB :: String
codeB  = "  generatePixel 0.3 >>= \\p -> print p\n"
codeC  = "  generatePixel 0.7 >>= \\p -> print p\n"
codeAB = "  generatePixel 0.5 >>= \\p -> print p\n"

grammar :: [Production]
grammar =
  [ P S' [NT S, T "$"]               (\[s,_]   -> "main :: IO ()\nmain = do\n" <> s)
  , P S  [NT A, T "a", NT S]         (\[a,_,s] -> a <> s)
  , P S  [T "b"]                     (const codeB)
  , P A  [NT C, NT A, T "b"]         (\[c,a,_] -> c <> a)
  , P A  [NT B]                      (\[b]     -> b)
  , P B  [T "c", NT S, T "a"]        (\[_,s,_] -> codeC <> s)
  , P B  [Eps]                       (const "")
  , P C  [T "a"]                     (const codeAB)
  , P C  [T "b"]                     (const codeAB)
  ]

nonterms :: [NonTerm]
nonterms = [S',S,A,B,C]

type FirstSets = Map NonTerm (Set String)
type FollowSets = Map NonTerm (Set String)
type Key = (NonTerm,String)
type ParseTable = Map Key Production

aempty :: Set String; aempty = S.empty

firstSets :: FirstSets
firstSets = fixedPoint step (M.fromList [(n,aempty) | n<-nonterms])
  where
    step tbl = foldl upd tbl grammar
      where upd acc (P nt β _) = M.insertWith S.union nt (firstSeq β tbl) acc

firstSeq :: [Symbol] -> FirstSets -> Set String
firstSeq []          _   = S.singleton ""
firstSeq (Eps:_)     _   = S.singleton ""
firstSeq (T t:_)     _   = S.singleton t
firstSeq (NT n:β) tbl =
  let f = M.findWithDefault aempty n tbl
  in if S.member "" f then S.delete "" f `S.union` firstSeq β tbl else f

followSets :: FollowSets
followSets = fixedPoint step start
  where
    start = M.insert S' (S.singleton "$") (M.fromList [(n,aempty) | n<-nonterms])
    step tbl = foldl proc tbl grammar
      where
        proc acc (P nt β _) = foldl (prop nt) acc (zip β (map tail $ tails β))
        prop pnt acc (NT a, suf) =
          let fβ = firstSeq suf firstSets
              acc1 = M.insertWith S.union a (S.delete "" fβ) acc
          in if S.member "" fβ || null suf
               then M.insertWith S.union a (M.findWithDefault aempty pnt tbl) acc1
               else acc1
        prop _ acc _ = acc

parseTable :: ParseTable
parseTable = foldl add M.empty grammar
  where
    add acc p@(P nt β _) = foldl ins acc sels
      where
        fβ = firstSeq β firstSets
        sels = if S.member "" fβ
                 then S.delete "" fβ `S.union` M.findWithDefault aempty nt followSets
                 else fβ
        ins m tok = case M.lookup (nt,tok) m of
                      Nothing -> M.insert (nt,tok) p m
                      Just _  -> error $ "LL(1) conflict at " ++ show (nt,tok)

type Result = (String,[String])

splitTokens :: String -> Either String [String]
splitTokens s = 
  let invalid = filter (`notElem` "abc") s
  in if null invalid 
     then Right $ map (:[]) s
     else Left $ "Invalid characters found: " ++ show invalid


parseS :: [String] -> [Result]
parseS toks = s_b toks ++ s_AaS toks
  where
    -- S → b
    s_b ("b":xs) = [(codeB,xs)]
    s_b _        = []
    -- S → A a S
    s_AaS ts =
      [ (aRes <> sRes, rest2)
      | (aRes, rest1) <- parseA ts
      , ("a":afterA)  <- [rest1]
      , (sRes, rest2) <- parseS afterA
      ]


parseA :: [String] -> [Result]
parseA ts = a_CAb ts ++ a_B ts
  where
    a_CAb xs =
      [ (cRes <> aRes, rest3)
      | (cRes, rest1) <- parseC xs
      , (aRes, rest2) <- parseA rest1
      , ("b":rest3)   <- [rest2] ]
    a_B = parseB


parseB :: [String] -> [Result]
parseB ("c":xs) =
  [ (codeC <> sRes, rest2)
  | (sRes, rest1) <- parseS xs
  , ("a":rest2)   <- [rest1] ]
parseB ts = [("",ts)]     


parseC :: [String] -> [Result]
parseC ("a":xs) = [(codeAB,xs)]
parseC ("b":xs) = [(codeAB,xs)]
parseC _        = []


parseTokens :: [String] -> Maybe String
parseTokens ts =
  case [c | (c,[]) <- parseS ts] of
    (r:_) -> Just r
    _     -> Nothing

parse :: String -> Either String String
parse inp = do
  tokens <- splitTokens inp
  case parseTokens tokens of
    Just c  -> Right c
    Nothing -> Left "String does not belong to grammar."


printFirstSets :: IO ()
printFirstSets = mapM_ pr (M.toList firstSets)
  where pr (n,s) = putStrLn $ show n ++ " : " ++ show (S.map f s)
        f "" = "eps"; f x = x

printFollowSets :: IO ()
printFollowSets = mapM_ (\(n,s)->putStrLn $ show n ++ " : " ++ show s)
                        (M.toList followSets)

printParseTable :: IO ()
printParseTable = mapM_ pr (M.toList parseTable)
  where pr ((nt,tok),P _ β _) =
          putStrLn $ show nt ++ ", '" ++ tok ++ "' => " ++ show β


genStrings :: Int -> [[String]]
genStrings depth = S.toList $ explore depth [[NT S]]
  where
    isNT (NT _) = True; isNT _ = False
    isT  (T _)  = True; isT  _ = False
    toTok (T x) = [x];  toTok _ = []

    explore 0 ss = S.fromList [concatMap toTok s | s<-ss, all isT s]
    explore k ss = explore (k-1) (ss >>= expand)

    expand s = case break isNT s of
                 (_,[])            -> [s]
                 (pre, NT nt:suf)  -> [pre ++ β ++ suf | β<-alts nt]

    alts S' = [[NT S, T "$"]]
    alts S  = [[NT A, T "a", NT S],[T "b"]]
    alts A  = [[NT C, NT A, T "b"],[NT B]]
    alts B  = [[T "c", NT S, T "a"],[]]
    alts C  = [[T "a"],[T "b"]]

fixedPoint :: Eq a => (a->a)->a->a
fixedPoint f x = let x' = f x in if x'==x then x else fixedPoint f x'

saveGeneratedStringsToFile :: Int -> IO ()
saveGeneratedStringsToFile d = writeFile "generated_strings.txt"
                             . unlines . map concat $ genStrings d

loadGeneratedStrings :: IO [[String]]
loadGeneratedStrings = do
  e <- doesFileExist "generated_strings.txt"
  if e then map (map (:[])) . lines <$> readFile "generated_strings.txt"
       else pure []

genInteract :: IO ()
genInteract = do
  prompt "Depth? "
  dStr <- getLine
  case reads dStr of
    [(d,"")] | d>0 -> mapM_ (putStrLn . concat) (genStrings d)
                       >> saveGeneratedStringsToFile d
    _              -> putStrLn "Not a positive integer."


data Pixel = Pixel { x :: Int, y :: Int, color :: String } deriving (Show)

generatePixel :: Double -> IO Pixel
generatePixel prob = do
  x <- randomRIO (0, 400)
  y <- randomRIO (0, 400)
  let r = floor $ prob * 255
      g = floor $ (1 - prob) * 255
      b = floor $ (prob + 0.5) * 127
  pure $ Pixel x y $ "rgb(" ++ show r ++ "," ++ show g ++ "," ++ show b ++ ")"


clearCanvas :: UI.Canvas -> UI ()
clearCanvas = UI.clearCanvas

data PixelPoint = PixelPoint { px :: Int, py :: Int, pointColor :: String }
                deriving (Eq, Show)
type Node = (Int,Int)

drawPoint :: UI.Canvas -> PixelPoint -> UI ()
drawPoint ctx (PixelPoint x y col) = do
  let gx = (x `div` 10) * 10
      gy = (y `div` 10) * 10
  ctx # set' UI.fillStyle (UI.htmlColor col :: UI.FillStyle)
  _   <- UI.fillRect (fromIntegral gx, fromIntegral gy) 10 10 ctx
  pure ()

drawPath :: UI.Canvas -> [Node] -> UI ()
drawPath _   []   = pure ()
drawPath ctx path = do
  ctx # set' UI.fillStyle (UI.htmlColor "rgba(0,0,255,0.3)" :: UI.FillStyle)
  mapM_ (\(x,y)-> UI.fillRect (fromIntegral x,fromIntegral y) 10 10 ctx
                   >> liftIO (threadDelay 100000)) path

drawGrid :: UI.Canvas -> UI ()
drawGrid ctx = do
  ctx # set' UI.strokeStyle "lightgray"
  ctx # set' UI.lineWidth 1
  forM_ [0,10..400] $ \x -> UI.beginPath ctx >> UI.moveTo (fromIntegral x,0) ctx
                                         >> UI.lineTo (fromIntegral x,400) ctx
                                         >> UI.stroke ctx
  forM_ [0,10..400] $ \y -> UI.beginPath ctx >> UI.moveTo (0,fromIntegral y) ctx
                                         >> UI.lineTo (400,fromIntegral y) ctx
                                         >> UI.stroke ctx

data Wall = Wall { wx :: Int, wy :: Int } deriving (Eq, Ord, Show)

randWall :: Set Node -> IO Wall
randWall busy = pick
  where
    pick = do
      gx <- randomRIO (0,39)
      gy <- randomRIO (0,39)
      let n = (gx*10,gy*10)
      if S.member n busy then pick else pure (Wall (fst n) (snd n))

generateWalls :: Int -> [Node] -> IO [Wall]
generateWalls n occ = go n (S.fromList occ) []
  where
    go 0 _ acc = pure acc
    go k busy acc = do
      w@(Wall x y) <- randWall busy
      go (k-1) (S.insert (x,y) busy) (w:acc)

moore :: Node -> [Node]
moore (x,y) = [(x+dx,y+dy)
              | dx<-[-10,0,10], dy<-[-10,0,10], (dx,dy)/=(0,0)]

inField :: Node -> Bool
inField (x,y) = x>=0 && x<=390 && y>=0 && y<=390

bfs :: Set Node -> Node -> Node -> Maybe [Node]
bfs walls start goal = go S.empty (S.singleton start) M.empty
  where
    go _   f _ | S.null f = Nothing
    go vis f prev
      | goal `S.member` f = Just (restore goal prev)
      | otherwise         = go vis' f' prev'
      where
        vis'  = vis `S.union` f
        neigh n = filter (\p->inField p && S.notMember p walls && S.notMember p vis')
                         (moore n)
        pairs = [ (p,n) | n<-S.toList f, p<-neigh n ]
        f'    = S.fromList (map fst pairs)
        prev' = foldl (\m (p,n)->M.insertWith (const id) p n m) prev pairs
    restore n pr | n==start = [n]
                 | otherwise = n : restore (pr M.! n) pr

data GridCell = GridCell { cellX :: Int, cellY :: Int, cellColor :: String }
              deriving (Eq, Show)

pointsToGrid :: [PixelPoint] -> [GridCell]
pointsToGrid = map (\(PixelPoint x y c)
                     -> GridCell ((x`div`10)*10) ((y`div`10)*10) c)

single :: String -> IO [PixelPoint]
single col = do x<-randomRIO (0,39); y<-randomRIO (0,39)
                pure [PixelPoint (x*10) (y*10) col]

double :: String -> IO [PixelPoint]
double col = do x1<-randomRIO (0,39); y1<-randomRIO (0,39)
                x2<-randomRIO (0,39); y2<-randomRIO (0,39)
                pure [ PixelPoint (x1*10) (y1*10) col
                     , PixelPoint (x2*10) (y2*10) col ]

generatePointForToken :: String -> IO [PixelPoint]
generatePointForToken "b"  = single "blue"
generatePointForToken "c"  = single "green"
generatePointForToken "a"  = single "orange"
generatePointForToken "ab" = double "red"
generatePointForToken _    = pure []

generatePointsFromGrammar :: String -> IO [PixelPoint]
generatePointsFromGrammar = fmap concat
                          . mapM generatePointForToken
                          . map (:[])

generatePattern :: String -> UI.Canvas -> UI ()
generatePattern str ctx =
  case parse str of
    Left err  -> runFunction $ ffi "alert(%1)" err
    Right code -> do
      clearCanvas ctx
      drawGrid ctx
      pts <- liftIO $ generatePointsFromGrammar str
      mapM_ (drawPoint ctx) pts

      let cells   = pointsToGrid pts
          busy    = [(cellX c,cellY c) | c<-cells]
      walls <- liftIO $ generateWalls 40 busy
      let wallSet = S.fromList [(wx w,wy w) | w<-walls]
      forM_ walls $ \(Wall x y) -> do
        ctx # set' UI.fillStyle (UI.htmlColor "black" :: UI.FillStyle)
        _ <- UI.fillRect (fromIntegral x,fromIntegral y) 10 10 ctx
        pure ()

      let gridPts = map (\c -> (cellX c,cellY c)) cells
          pairs   = zip gridPts (tail gridPts)
      paths <- forM pairs $ \(a,b) ->
                 case bfs wallSet a b of
                   Nothing -> runFunction (ffi "alert(%1)"
                                           ("No path between "++show a++
                                            " and "++show b)) >> pure []
                   Just p  -> pure p
      mapM_ (drawPath ctx) paths
      runFunction $ ffi "alert(%1)" code


setup :: Window -> UI ()
setup window = do
  return window # set title "Pixel Pattern Generator"
  canvas <- UI.canvas # set UI.width 400
                      # set UI.height 400
                      # set style [("border","1px solid black")]

  input <- UI.input   # set UI.type_ "text"
                      # set (attr "placeholder") "Enter pattern (e.g. bbcbabbab)"
  btnGen   <- UI.button #+ [string "Generate Pattern"]
  btnClear <- UI.button #+ [string "Clear"]
  btnFst   <- UI.button #+ [string "Show FIRST sets"]
  btnFol   <- UI.button #+ [string "Show FOLLOW sets"]
  btnTbl   <- UI.button #+ [string "Show parse table"]
  btnStrs  <- UI.button #+ [string "Generate Strings"]
  depthInp <- UI.input  # set UI.type_ "number"
                        # set (attr "placeholder") "Depth (1-50)"
                        # set (attr "min") "1" # set (attr "max") "50"
                        # set (attr "value") "3"

  getBody window #+
    [ column [ element input, element btnGen, element btnClear
             , element canvas
             , element btnFst, element btnFol, element btnTbl
             , element depthInp, element btnStrs ] ]

  on UI.click btnGen   $ const $ do p <- get value input
                                    generatePattern p canvas
  on UI.click btnClear $ const $ clearCanvas canvas
  on UI.click btnFst   $ const $ liftIO (printFirstSets  >> pause)
  on UI.click btnFol   $ const $ liftIO (printFollowSets >> pause)
  on UI.click btnTbl   $ const $ liftIO (printParseTable >> pause)
  on UI.click btnStrs  $ const $ do
    dStr <- get value depthInp
    case reads dStr of
      [(d,"")] | d>0 && d<=50 -> liftIO $ do
                                   putStrLn ("Depth "++show d++":")
                                   mapM_ (putStrLn.concat) (genStrings d)
                                   saveGeneratedStringsToFile d
                                   pause
      _ -> runFunction $ ffi "alert(%1)" "Enter depth 1-50!"