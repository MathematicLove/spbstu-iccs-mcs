type Point = (Double, Double)
vR :: [Point]
vR = [(0, 0), (1, 0), (0.5, sqrt 3 / 2)]  
avg :: Point -> Point -> Point
avg (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)
serpinka :: Int -> [Point] -> [[Point]]

serpinka 0 treangle = [treangle]   
serpinka n [p1, p2, p3] = 
    let m1 = avg p1 p2  
        m2 = avg p2 p3   
        m3 = avg p1 p3  
        lowertreangles = serpinka (n-1) [p1, m1, m3] ++
                         serpinka (n-1) [m1, p2, m2] ++
                         serpinka (n-1) [m3, m2, p3]
    in lowertreangles

printserpinka :: [[Point]] -> IO ()
printserpinka [] = return ()
printserpinka (p:ps) = do
    putStrLn $ show p ++ ","
    printserpinka ps

main :: IO ()
main = do
    let initialtreangle = vR   
    let depth = 6   
    let serpinkaPoints = serpinka depth initialtreangle   
    printserpinka serpinkaPoints   
