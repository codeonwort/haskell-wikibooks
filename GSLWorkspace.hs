{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module GSLWorkspace (Workspace, createWorkspace) where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.ForeignPtr

data Workspace
foreign import ccall unsafe "gsl/gsl_integration.h gsl_integration_workspace_alloc"
    c_ws_alloc :: CSize -> IO (Ptr Workspace)
foreign import ccall unsafe "gsl/gsl_integration.h &gsl_integration_workspace_free"
    c_ws_free :: FunPtr ( Ptr Workspace -> IO () )

createWorkspace :: CSize -> IO (Maybe (ForeignPtr Workspace) )
createWorkspace size = do
    ptr <- c_ws_alloc size
    if ptr /= nullPtr
        then do
            foreignPtr <- newForeignPtr c_ws_free ptr
            return $ Just foreignPtr
        else
            return Nothing
