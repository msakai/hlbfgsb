module Numeric.Lbfgsb
    ( minimize
    , minimizeV
    ) where

import Control.Arrow hiding (loop)
import Control.Monad
import Data.Char
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Storable as SV
import Foreign hiding (unsafePerformIO)
import System.IO.Unsafe (unsafePerformIO)

readTask :: Int -> Ptr Word8 -> IO String
readTask n a = do
     s <- peekArray n a
     return $ map (chr . fromIntegral) s

expandConstraints :: [(Maybe Double, Maybe Double)]
                  -> ([Double], [Double], [Int])
expandConstraints cs = unzip3 . map conv $ cs
  where
    conv (Nothing, Nothing) = (47, 47, 0)
    conv (Just x, Nothing) = (x, 47, 1)
    conv (Just x, Just y) = (x, y, 2)
    conv (Nothing, Just y) = (47, y, 3)

vectorize :: ([Double] -> (Double, [Double])) -> SV.Vector Double -> (Double, SV.Vector Double)
vectorize fg = second V.fromList . fg . V.toList

unvectorize :: (SV.Vector Double -> (Double, SV.Vector Double)) -> [Double] -> (Double, [Double])
unvectorize fg = second V.toList . fg . V.fromList

minimizeV :: Int                                               -- m - number of past iterations
          -> Double                                            -- factr - accuracy factor e.g. 1e3
          -> Double                                            -- pgtol - gradiant tolerance e.g. 1e-10
          -> SV.Vector Double                                  -- x
          -> [(Maybe Double, Maybe Double)]                    -- bounds
          -> (SV.Vector Double -> (Double, SV.Vector Double))  -- fg
          -> SV.Vector Double
minimizeV m factr pgtol x bounds fg = unsafePerformIO $ minimizeIO (-1) m factr pgtol x bounds fg

minimize :: Int
         -> Double
         -> Double
         -> [Double]
         -> [(Maybe Double, Maybe Double)]
         -> ([Double] -> (Double, [Double]))
         -> [Double]
minimize m factr pgtol x bounds fg = V.toList $ minimizeV m factr pgtol (V.fromList x) bounds (vectorize fg)

minimizeIO :: Int                                               -- verbosity
           -> Int                                               -- m
           -> Double                                            -- factr
           -> Double                                            -- pgtol
           -> SV.Vector Double                                  -- x
           -> [(Maybe Double, Maybe Double)]                    -- bounds
           -> (SV.Vector Double -> (Double, SV.Vector Double))  -- fg
           -> IO (SV.Vector Double)
minimizeIO verbosity m factr pgtol x bounds fg =
    with n $ \n' ->
    with m $ \m' ->
    withArray (V.toList x) $ \x' ->
    withArray ls $ \l ->
    withArray us $ \u ->
    withArray nbds $ \nbd ->
    alloca $ \f ->
    allocaArray n $ \g ->
    with factr $ \factr' ->
    with pgtol $ \pgtol' ->
    allocaArray ((2*m+5)*n + 11*m*m + 8*m) $ \wa ->
    allocaArray (3*n + 470) $ \iwa ->
    withArray sTART $ \task ->
    with verbosity $ \iprint ->
    allocaArray 60 $ \csave ->
    allocaArray 4 $ \lsave ->
    allocaArray 44 $ \isave ->
    allocaArray 29 $ \dsave -> do
    res <- loop n' m' x' l u nbd f g factr' pgtol' wa iwa task iprint csave lsave isave dsave
    return res
  where
    loop n' m' x' l u nbd f g factr' pgtol' wa iwa task iprint csave lsave isave dsave = loop'
      where
        loop' = do
            setulb_ n' m' x' l u nbd f g factr' pgtol' wa iwa task iprint csave lsave isave dsave 60 60
            when (verbosity > 1) $ print =<< readTask 50 task
            readTask 5 task >>= \t -> case t of
                'F':'G':_ -> updateFg >> loop'
                "NEW_X" -> (when (verbosity > 1) $ print =<< readDoubles n x') >>
                           updateFg >> loop'
                _ -> readDoubles n x'
        updateFg = do
            xs <- readDoubles n x'
            let (fnew, gnew) = fg xs
            poke f fnew
            pokeArray g (V.toList gnew)

    n = V.length x
    readDoubles :: Int -> Ptr Double -> IO (SV.Vector Double)
    readDoubles n' ds = (return . V.fromList) =<< peekArray n' ds
    (ls, us, nbds) = expandConstraints . take n $ bounds ++ repeat (Nothing, Nothing)
    sTART = map (fromIntegral . ord) . take 60 $ "START" ++ repeat ' '


foreign import ccall setulb_
    :: Ptr Int     -- n  - number of variables
    -> Ptr Int     -- m  - memory - number of corrections
    -> Ptr Double  -- x[n] - estimate
    -> Ptr Double  -- l[n] - lower bound
    -> Ptr Double  -- u[n] - upper bound
    -> Ptr Int     -- nbd[n] - has bound (lb | up << 1)
    -> Ptr Double  -- f - function value
    -> Ptr Double  -- g[n] - gradient
    -> Ptr Double  -- factr - accuracy factor
    -> Ptr Double  -- pgtol - stop condition gradient tolerance
    -> Ptr Double  -- wa[(2m+5)n + 11m^2 + 8m] - working array
    -> Ptr Int     -- iwa[3n] - working array
    -> Ptr Word8   -- task[60]
    -> Ptr Int     -- iprint - output
    -> Ptr Word8   -- csave[60] - char working array
    -> Ptr Bool    -- lsave[4]
    -> Ptr Int     -- isave[44]
    -> Ptr Double  -- dsave[29]
    -> Int         -- task length
    -> Int         -- csave length
    -> IO ()
