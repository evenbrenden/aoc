module Common where

import System.IO
import Text.Trifecta

handleResult (Success is) = is
handleResult (Failure f) = error $ show f

load :: String -> IO String
load path = do
    handle <- openFile path ReadMode
    hGetContents handle
