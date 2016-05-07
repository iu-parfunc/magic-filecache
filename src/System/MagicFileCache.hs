

module System.MagicFileCache
    ( loadFile
    , loadAction
    ) where


import Control.DeepSeq
import Data.Compact
import Data.ByteString.Lazy as B
import System.IO.Unsafe
import System.Directory
import System.FilePath
import System.Random
import Data.Word

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
loadFile :: NFData a => (ByteString -> a) -> FilePath -> IO (Compact a)
loadFile fn =
 loadAction (\p -> do bs <- B.readFile p 
                      return $ fn bs)

-- | Some library functions load a file directly, and cannot work from
-- a ByteString.  Memoize such a load action.
--
-- `loadFile` can easily be written in terms of `loadAction`.
loadAction :: NFData a => (FilePath -> IO a) -> FilePath -> IO (Compact a)
loadAction act pth =
 do canon <- canonicalizePath pth
    relcanon <- withCurrentDirectory "/" $
                 makeRelativeToCurrentDirectory canon
    let cache = cachedir </> relcanon
    b <- doesFileExist cache
    uid <- randomIO :: IO Word64
    let temploc = cache ++ "_"++show uid    
    error$ "FINISHME: load cache file: "++show cache++" exists "++show b

-- TODO: ^^ various levels of safety checking or fingerprinting can be
-- considered to control the update-detection algorithm.


clearCache :: FilePath -> IO Bool
clearCache = undefined

