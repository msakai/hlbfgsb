import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit

import Test.HUnit

import Numeric.Lbfgsb

import Control.Monad
import Control.Arrow
import qualified Data.Vector.Generic as V

main = defaultMain tests

tests = [
        testGroup "Basic tests" [
                testCase "list" test1,
                testCase "vector" test2
            ]
{- perhaps make this work some day
 - or create different tests?
        testGroup "Tests from Nocedal" [
                testCase "n=25,m=5" (testNoced 25)
            ] -}
    ]

bisquare [x,y] = ((x-4)*(x-3) + (y-2)*(y-1), [(x-4)+(x-3), (y-2)+(y-1)])

vectorize fg = second V.fromList . fg . V.toList

test1 = [3.5, 1.5] @=? minimize 3 1e3 1e-20 [47, 47] [] bisquare

test2 = [3.5, 1.5] @=? (V.toList $ minimizeV 3 1e3 1e-20 (V.fromList [47, 47]) [] (vectorize bisquare))

testNoced n = [] @=? (minimize 5 1e3 1e-5 (start n) (bounds n) (calcF n &&& calcG n))

bnd i |     odd i = (Just    1, Just 100)
      |    even i = (Just (-1), Just 100)

bounds n = map bnd [1..n]

start n = replicate n 3

{-
init        f=.25d0*( x(1)-1.d0 )**2
do12        do 20 i=2, n
step           f = f + ( x(i)-x(i-1 )**2 )**2
continue    continue
end         f = 4.d0*f
-}
calcF n x' = init
  where
    x = 0:x'
    init         = do1 (0.25 * ((x !! 1) - 1) ** 2)
    do1      f   = do2 f 2
    do2      f i
     | i <= n    = step f i
     | otherwise = end  f
    step     f i = continue (f + ( (x!!i)-(x !! (i-1) )**2 )**2) i
    continue f i = do2 f (i+1)
    end      f = 4 * f

{-
init     t1=x(2)-x(1)**2
line1    g(1)=2.d0*(x(1)-1.d0)-1.6d1*x(1)*t1
do12     do 22 i=2,n-1
step1       t2=t1
step2       t1=x(i+1)-x(i)**2
step3       g(i)=8.d0*t2-1.6d1*x(i)*t1
continue continue
end      g(n)=8.d0*t1
-}


xs =!! (i, x) = take i (xs ++ repeat 0) ++ (x : drop (i+1) xs)

calcG n x' = init
  where
    x = 0:x'
    init               = line1 ((x!!2)-(x!!1)**2)
    line1    t1        = do1 t1 ([] =!! (1,2*((x!!1)-1)-16*(x!!1)*t1))
    do1      t1 g      = do2 t1 g 2
    do2      t1 g i
           | i <= n-1  = step1 t1 g i
           | otherwise = end t1 g
    step1    t1 g i    = step2 t1 g i t1
    step2    t1 g i t2 = step3 ((x !! (i+1)) - (x !! i) ** 2) g i t2
    step3    t1 g i t2 = continue t1 (g =!! (i, (8*t2-16*(x!!i)*t1))) i
    continue t1 g i    = do2 t1 g (i+1)
    end      t1 g = tail $ (g =!! (n, 8*t1))
