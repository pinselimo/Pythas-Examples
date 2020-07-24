{-# LANGUAGE ForeignFunctionInterface #-}
module Wonderland_hasky_ffi where

import qualified Wonderland

import Foreign.C.Types
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Marshal.Alloc (free)
import Foreign.Storable (peek)
import Control.Monad (liftM2, liftM3, liftM4)
import Foreign.C.Structs
import Foreign.Pythas.Array
import Foreign.Pythas.List
import Foreign.Pythas.String
import Foreign.Pythas.Tuples


foreign export ccall runWonderland :: CWString -> CInt -> IO (CArray (CTuple4  (CDouble) (CDouble) (CDouble) (CDouble)))
runWonderland a b =  ( ( (peekCWString a) >>=
     (\ a ->  (return (Wonderland.runWonderland a (fromIntegral b))))) >>=
     (\ res ->  ( (mapM (\ res ->  ( ( (\ ( a,  b,  c,  d) ->  (return ((,,,) (CDouble a) (CDouble b) (CDouble c) (CDouble d)))) res) >>=
     (\ res ->  (newTuple4 res)))) res) >>=
     (\ res ->  (newArray res)))))

foreign export ccall runWonderlandFinalizer :: CArray (CTuple4  (CDouble) (CDouble) (CDouble) (CDouble)) -> IO ()
runWonderlandFinalizer x =  ( (peekArray x) >>=
     (\ x ->  (mapM (free) x))) >>
     (freeArray x)
