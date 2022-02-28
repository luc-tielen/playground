
module Main ( main ) where

import Language.Souffle.Interpreted as Souffle
import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Control.Exception

foreign import ccall "add" foreignAdd :: CInt -> CInt -> CInt

data Example

foreign import ccall "init_example" createExample :: IO (Ptr Example)
foreign import ccall "&deinit_example" destroyExample :: FunPtr (Ptr Example -> IO ())
foreign import ccall "do_stuff_example" doStuffExample :: Ptr Example -> CInt -> IO CBool

mkExample :: IO (ForeignPtr Example)
mkExample = mask_ $ do
  ptr <- createExample
  newForeignPtr destroyExample ptr

main :: IO ()
main = do
  print $ foreignAdd 3 4

  -- obj <- createExample
  -- print =<< doStuffExample obj 100
  -- destroyExample obj

  ptr <- mkExample
  withForeignPtr ptr $ \p -> print =<< doStuffExample p 10
