{-# LANGUAGE ForeignFunctionInterface #-}

module Qag ( qag,
    gauss15,
    gauss21,
    gauss31,
    gauss41,
    gauss51,
    gauss61 ) where

import GSLWorkspace

import Data.Maybe (isNothing, fromJust)
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe

#include <gsl/gsl_math.h>
#include <gsl/gsl_integration.h>

foreign import ccall unsafe "gsl/gsl_errno.h gsl_strerror"
    c_error_string :: CInt -> IO CString

foreign import ccall unsafe "gsl/gsl_errno.h gsl_set_error_handler_off"
    c_deactivate_gsl_error_handler :: IO ()

foreign import ccall "wrapper"
    makeFunPtr :: CFunction -> IO (FunPtr CFunction)

foreign import ccall safe "gsl/gsl_integration.h gsl_integration_qag"
    c_qag :: Ptr GslFunction -- Allocated GSL function structure
          -> CDouble         -- Start interval
          -> CDouble         -- End interval
          -> CDouble         -- Absolute tolerance
          -> CDouble         -- Relative tolerance
          -> CSize           -- Maximum number of subintervals
          -> CInt            -- Type of Gauss-Kronrod rule
          -> Ptr Workspace   -- GSL integration workspace
          -> Ptr CDouble     -- Result
          -> Ptr CDouble     -- Computation error
          -> IO CInt         -- Exit code

newtype IntegrationRule = IntegrationRule { rule :: CInt }
#{enum IntegrationRule, IntegrationRule,
    gauss15 = GSL_INTEG_GAUSS15,
    gauss21 = GSL_INTEG_GAUSS21,
    gauss31 = GSL_INTEG_GAUSS31,
    gauss41 = GSL_INTEG_GAUSS41,
    gauss51 = GSL_INTEG_GAUSS51,
    gauss61 = GSL_INTEG_GAUSS61
}

type CFunction = CDouble -> Ptr () -> CDouble

data GslFunction = GslFunction (FunPtr CFunction) (Ptr ())
instance Storable GslFunction where
    sizeOf _ = (#size gsl_function)
    alignment _ = alignment (undefined :: Ptr ())
    peek ptr = do
        function <- (#peek gsl_function, function) ptr
        return $ GslFunction function nullPtr
    poke ptr (GslFunction fun nullPtr) = do
        (#poke gsl_function, function) ptr fun

makeCfunction :: (Double -> Double) -> (CDouble -> Ptr () -> CDouble)
makeCfunction f = \x voidpointer -> realToFrac $ f (realToFrac x)

qag :: IntegrationRule                 -- Algorithm type
    -> Int                             -- Step limit
    -> Double                          -- Absolute tolerance
    -> Double                          -- Relative tolerance
    -> (Double -> Double)              -- Function to integrate
    -> Double                          -- Integration interval start
    -> Double                          -- Integration interval end
    -> Either String (Double, Double)  -- Result and (absolute) error estimate
qag gauss steps abstol reltol f a b = unsafePerformIO $ do
    c_deactivate_gsl_error_handler
    ws <- createWorkspace (fromIntegral steps)
    if isNothing ws
        then return $ Left "GSL could not allocate workspace"
        else withForeignPtr (fromJust ws) $ \workspacePtr -> do
            fPtr <- makeFunPtr $ makeCfunction f
            alloca $ \gsl_f -> do
                poke gsl_f (GslFunction fPtr nullPtr)
                alloca $ \resultPtr -> do
                    alloca $ \errorPtr -> do
                        status <- c_qag gsl_f
                                        (realToFrac a)
                                        (realToFrac b)
                                        (realToFrac abstol)
                                        (realToFrac reltol)
                                        (fromIntegral steps)
                                        (rule gauss)
                                        workspacePtr
                                        resultPtr
                                        errorPtr
                        freeHaskellFunPtr fPtr
                        if status /= 0
                            then do
                                c_errormsg <- c_error_string status
                                errormsg <- peekCString c_errormsg
                                return $ Left errormsg
                            else do
                                c_result <- peek resultPtr
                                c_error <- peek errorPtr
                                let result = realToFrac c_result
                                let error = realToFrac c_error
                                return $ Right (result, error)

