{-# LANGUAGE ScopedTypeVariables #-}

module System.MagicFileCache
    ( loadFile
    , loadWAction
    ) where


import Control.DeepSeq
import Control.Exception
import Data.Compact
import Data.ByteString.Lazy as B
import System.IO.Unsafe
import System.IO
import System.Directory
import System.FilePath
import System.Random
import Data.Word
import Prelude as P
import Data.Compact.Files 
import Data.Typeable

-- loadFile :: NFData a => FilePath -> IO a -> IO (Compact a)

cachedir :: FilePath
cachedir = unsafePerformIO $ getAppUserDataDirectory "magic-cache"

-- | Load a file from disk, memoizing the load as a CNF on Disk.
--
--   That is, after the first call of this function on a given file,
--   subsequent loads will go faster.
--
--   However, there is a side condition to calling this function that
--   requires significant care.  A given file must only ever be used
--   with a SINGLE parse function.  Without an exact way of naming the
--   parse computations, we have no good way to associate them with
--   the cached CNF values.
loadFile :: (Typeable a, NFData a)
         => (ByteString -> a) -> FilePath -> IO (Compact a)
loadFile fn =
 loadWAction (\p -> do bs <- B.readFile p 
                       return $ fn bs)

-- | Some library functions load a file directly, and cannot work from
-- a ByteString.  Memoize such a load action.
--
-- `loadFile` can easily be written in terms of `loadWAction`.
loadWAction :: (Typeable a, NFData a)
           => (FilePath -> IO a) -> FilePath -> IO (Compact a)
loadWAction act pth =
 do canon <- canonicalizePath pth
    relcanon <- withCurrentDirectory "/" $
                 makeRelativeToCurrentDirectory canon
    let cache = cachedir </> relcanon <.> "cnf"
    -- b <- doesFileExist cache -- for now they are directories!
    b <- doesDirectoryExist cache
    if b then do
      unsafeMapCompactFile cache
     else do
      hPutStrLn stderr $ "loadWAction: no cache found for "++pth++".  Populating..."
      uid <- randomIO :: IO Word64
      let temploc = cache ++ "_"++show uid
      -- TODO: Can optionally fork process here:
      res <- act pth
      -- When we're writing to disk it's worth it to preserve sharing!
      -- As an example, parsed Haskell files are 3X bigger with no sharing:
      c <- newCompact (32*1024) res
      writeCompactFile temploc c
-- !!!TODO!!! -- implement time stamp checking"

      -- There should be a more elegant way to do this:
      b1 <- doesFileExist temploc
      catch (-- Benign race to do the rename:
             if b1
               then renameFile      temploc cache
               else renameDirectory temploc cache)
            -- This is best-effort, ignore any problems:
            (\(e::SomeException) ->
              -- This is lame, and doesn't seem to match System.Directory description.
              do -- P.putStrLn $ "TEMPDBG: Exception caught during rename: "++ show e
                 tryNoFail $ removeDirectoryRecursive temploc
                 return ())
      return c

tryNoFail :: IO () -> IO ()
tryNoFail io = catch io (\(_e::SomeException) -> return ())

-- TODO: ^^ various levels of safety checking or fingerprinting can be
-- considered to control the update-detection algorithm.


clearCache :: FilePath -> IO Bool
clearCache = undefined

