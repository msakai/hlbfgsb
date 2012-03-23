import Control.Arrow (second, (***))
import qualified Data.Vector.Generic as V
import Numeric.GSL.Minimization
import System.IO
import System.IO.Unsafe

import Lbfgsb

fg [x,y] = ((x-4)*(x-3) + (y-2)*(y-1), [(x-4)+(x-3), (y-2)+(y-1)])

vectorize fg' = second V.fromList . fg' . V.toList

neg = negate *** map negate

test :: IO ()
test = do
    hSetBuffering stdout NoBuffering
--    print =<< lbfgsb 3 1e3 1e-10 (V.fromList [47, 47]) [] (vectorize (neg . fg))
--    print $ minimize NMSimplex2 1e-6 100 [100,100] (fst . fg) [47,47]
    print =<< lbfgsb 5 1e7 1e-5 (V.fromList . replicate 25 $ 3) (map bnd [1..25]) (vectorize (p fg2))

bnd n |     odd n = (Just 1, Just 100)
      | otherwise = (Just 1, Just 100)

p f x = unsafePerformIO $ do
--    print r
    return r
  where r = f x

fg2 :: [Double] -> (Double, [Double])
fg2 xs = (f xs, g)
  where
    f xs@(x:xs')= ((0.25*(x-1)**2) + sm (zip xs' xs)) * 4
    sm :: [(Double, Double)] -> Double
    sm = sum . map (\(x2, x1) -> (x2 - x1**2)**2)
    g = map (\n -> let dx = xs !! n * 1e-8 in (f (atp dx n xs) - f xs) / dx) [0..(length xs - 1)]

    atp d n xs = zipWith (+) xs (replicate n 0 ++ d:repeat 0)
--    t = xs !! 1 - x ** 2 : (map (\x2 x1 -> x2 - x1 ** 2) . tail) . (zip xs' xs)
--    g = ((2*(x-1)-1.6*x*(t!!0)) : map (\t2 t1 x1 -> 8*t2 - 1.6*x1*t1) . zip3 t (tail t) $ g) ++ 8 * last t



main :: IO ()
main = test
