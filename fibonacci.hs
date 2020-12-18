{-# LANGUAGE ForeignFunctionInterface #-}

module Fibonacci where

import Foreign
import Foreign.C.Types

fibonacci :: (Integral a) => [a]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)

foreign export ccall fibonacci_c :: CInt -> Ptr CULLong -> Ptr CDouble -> IO CInt
fibonacci_c :: CInt -> Ptr CULLong -> Ptr CDouble -> IO CInt
fibonacci_c n intPtr dblPtr
    | badInt && badDouble = return 2
    | badInt              = do
        poke dblPtr dbl_result
        return 1
    | otherwise           = do
        poke intPtr (fromIntegral result)
        poke dblPtr dbl_result
        return 0
    where
        result = fibonacci !! (fromIntegral n)
        dbl_result = realToFrac result
        badInt = result > toInteger (maxBound :: CULLong)
        badDouble = isInfinite dbl_result
