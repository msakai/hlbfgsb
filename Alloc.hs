module Alloc

where

import Control.Monad
import qualified Foreign as F
import Foreign (Word8, peekArray, pokeArray)
import Foreign.Ptr
import Foreign.Storable

alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca = allocaArray 1

with :: Storable a => a -> (Ptr a -> IO b) -> IO b
with x = withArray [x]

c :: Int
c = 470

allocaArray :: Storable a => Int -> (Ptr a -> IO b) -> IO b
allocaArray n f = F.allocaArray (n+2*c) $ \ptr -> do
    let s = sizeOf (ptrType ptr)
	band = replicate (c * s) (47 :: Word8)
	begin = castPtr ptr
	data_ = ptr `plusPtr` (c * s) `asTypeOf` ptr
	end = castPtr $ data_ `plusPtr` (n * s) in do
    putStrLn $ "Alloc " ++ show n ++ " x " ++ show s
    pokeArray begin band
    pokeArray end band
    res <- f data_
    b <- peekArray (c * s) begin
    e <- peekArray (c * s) end
    when (b /= band || e /= band) $ do
	putStrLn "\n\n\nDetected invalid memory access!!!\n"
	print b
	print e
	putStrLn "\n\n\n"
    putStrLn $ "Free " ++ show n ++ " x " ++ show s
    return res
  where
    ptrType :: Ptr a -> a
    ptrType _ = undefined

withArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArray xs f = allocaArray (length xs) $ \ptr -> do
    pokeArray ptr xs
    f ptr
