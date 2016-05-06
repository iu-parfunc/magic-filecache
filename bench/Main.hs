{-# LANGUAGE RecordWildCards #-}

-- | This benchmark loads an interface file using the GHC API.

module Main where

import System.Environment

import GHC
import GHC.Paths ( libdir )
----------------------------
import LoadIface hiding (showIface)
import HscTypes
import DynFlags
import TcRnMonad
import BinIface
import SrcLoc
import ErrUtils
import Outputable
import HscMain (newHscEnv)

import Control.DeepSeq
import Data.Compact as C

-- Ugh:
instance NFData ModIface where
  rnf ifc = ()
    -- HACK: Super inefficient:
--    seq (length (show (pprModIface ifc))) ()

  -- rnf ModIface{..} =
  --   rnf mi_module `seq`
  --   ()

-- instance NFData Module where
-- FINISHME:

showIface :: HscEnv -> FilePath -> IO ()
showIface hsc_env filename = do
   -- skip the hi way check; we don't want to worry about profiled vs.
   -- non-profiled interfaces, for example.
   iface <- initTcRnIf 's' hsc_env () () $
       readBinIface IgnoreHiWay TraceBinIFaceReading filename
   let dflags = hsc_dflags hsc_env
   -- let sdoc = (pprModIface iface) a
   -- log_action dflags dflags NoReason SevDump noSrcSpan defaultDumpStyle (pprModIface iface)
   -- writeBinIface :: DynFlags -> FilePath -> ModIface -> IO ()

   -- LAME way to try for NF.  NOPE, doesn't work.
   writeBinIface dflags "./out.hi" iface
   putStrLn "IFACE LOADED!"
   c <- newCompact 4096 iface

   return ()
   -- writeBinIface :: DynFlags -> FilePath -> ModIface -> IO ()

showIface2 :: FilePath -> IO ()
showIface2 p =
  do
     runGhc (Just libdir) $ do
             dflags <- getSessionDynFlags
             setSessionDynFlags dflags             
             liftIO$ do he <- newHscEnv dflags
                        showIface he p


-- This file is 600K.  It takes 1.4 MB to pretty-print to a file.
main = do args <- getArgs
          let file = case args of
                      [f] -> f
                      [] -> "./Big.hi"
          putStrLn$ "Loading file: "++file
          showIface2 file
          putStrLn "Done."
