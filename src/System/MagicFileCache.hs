

module System.MagicFileCache
    ( loadFile
    ) where


import Control.DeepSeq
import Data.Compact
import Data.ByteString.Lazy as B
import System.IO.Unsafe
import System.Directory

-- loadFile :: NFData a => FilePath -> IO a -> IO (Compact a)

cachedir :: FilePath
cachedir = unsafePerformIO $ getAppUserDataDirectory "magic-cache"

-- | Load a file from disk, memoizing the load as a CNF on Disk.
--
--   That is, after the first call
loadFile :: NFData a => FilePath -> (ByteString -> a) -> IO (Compact a)
loadFile pth =
 do undefined

-- TODO: ^^ various levels of safety checking or fingerprinting can be
-- considered to control the update-detection algorithm.


clearCache :: FilePath -> IO Bool
clearCache = undefined

