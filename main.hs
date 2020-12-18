{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import System.IO.Unsafe
import Text.Printf

import Bessel
import Qag

-- foreign imports ------------------------

foreign import ccall unsafe "math.h sin"
    c_sin :: CDouble -> CDouble

foreign import ccall unsafe "stdlib.h rand"
    c_rand :: IO CUInt

foreign import ccall "stdlib.h srand"
    c_srand :: CUInt -> IO ()

foreign import ccall unsafe "gsl/gsl_math.h gsl_frexp"
    gsl_frexp :: CDouble -> Ptr CInt -> IO CDouble

-- haskell interfaces ---------------------

hs_sin :: Double -> Double
hs_sin = realToFrac . c_sin . realToFrac

frexp :: Double -> (Double, Int)
frexp x = unsafePerformIO $
    alloca $ \expptr -> do
        f <- gsl_frexp (realToFrac x) expptr
        e <- peek expptr
        return (realToFrac f, fromIntegral e)

-- main -----------------------------------

main = do
    -- sin
    putStrLn "-- Test: sin --"
    let testSin x = putStrLn $ printf "sin(%f) = %f" x (hs_sin x)
    testSin 0.0
    testSin 1.414
    -- rand
    let srand_seed = 0
    putStrLn "-- Test: rand --"
    let testRand = c_rand >>= \x -> putStrLn (printf "rand() = %d" (fromIntegral x :: Int))
    putStrLn $ printf "srand(%d)" (fromIntegral srand_seed :: Int)
    c_srand srand_seed
    testRand
    testRand
    testRand
    testRand
    -- frexp
    putStrLn "-- Test: frexp --"
    let x = 74825.6928
    let (f, e) = frexp x
    putStrLn $ printf "frexp(%f) = (%f, %d)" x f e
    putStrLn $ printf "%f * 2^%d = %f" f e (f * (2 ** (fromIntegral e)))
    -- qag
    let rule = gauss15
    let step_limit = 128
    let abstol = 0.0001
    let reltol = 0.0001
    let fn x = x
    let interval_start = 0.0
    let interval_end = 1.0
    let qag_result = qag rule step_limit abstol reltol fn interval_start interval_end
    case qag_result of
        Left err -> putStrLn $ printf "qag = %s" err
        Right (x, e) -> putStrLn $ printf "qag = %f (error=%f)" x e

