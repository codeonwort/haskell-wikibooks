{-# LANGUAGE ForeignFunctionInterface #-}

module Bessel (besselJn) where

import Foreign
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import System.IO.Unsafe

#include <gsl/gsl_sf_result.h>

foreign import ccall unsafe "gsl/gsl_bessel.h gsl_sf_bessel_Jn_e"
    c_besselJn :: CInt -> CDouble -> Ptr GslSfResult -> IO CInt

foreign import ccall unsafe "gsl/gsl_errno.h gsl_set_error_handler_off"
    c_deactivate_gsl_error_handler :: IO ()

foreign import ccall unsafe "gsl/gsl_errno.h gsl_strerror"
    c_error_string :: CInt -> IO CString

data GslSfResult = GslSfResult {
    gsl_value :: CDouble,
    gsl_error :: CDouble
}

instance Storable GslSfResult where
    sizeOf _ = (#size gsl_sf_result)
    alignment _ = alignment (undefined :: CDouble)
    peek ptr = do
        value <- (#peek gsl_sf_result, val) ptr
        error <- (#peek gsl_sf_result, err) ptr
        return GslSfResult { gsl_value = value, gsl_error = error }
    poke ptr (GslSfResult value error) = do
        (#poke gsl_sf_result, val) ptr value
        (#poke gsl_sf_result, err) ptr error

besselJn :: Int -> Double -> Either String (Double, Double)
besselJn n x = unsafePerformIO $
    alloca $ \gslSfPtr -> do
        c_deactivate_gsl_error_handler
        status <- c_besselJn (fromIntegral n) (realToFrac x) gslSfPtr
        if status == 0
            then do
                GslSfResult val err <- peek gslSfPtr
                return $ Right (realToFrac val, realToFrac err)
            else do
                error <- c_error_string status
                error_message <- peekCString error
                return $ Left ("GSL error: " ++ error_message)

