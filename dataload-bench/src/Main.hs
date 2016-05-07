{-#  LANGUAGE NamedFieldPuns #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE ScopedTypeVariables #-}
{-#  LANGUAGE ForeignFunctionInterface #-}
{-#  LANGUAGE CPP #-}
{-#  LANGUAGE BangPatterns #-}
{-#  LANGUAGE OverloadedStrings #-}

-- | Benchamrk script: no criterion.  One measurement per process run.

module Main where
import Control.Monad
import Control.DeepSeq
import Control.Exception
import Control.Concurrent
-- import qualified Data.Vector as V
import qualified Data.Array as A
import           Data.Typeable
import           Data.Word
import           Data.IORef
import qualified Data.Set as S
import           Data.Char (isSpace)
import           System.Environment
import           System.FilePath
import           System.Directory
import           System.IO
import           System.Mem
import           Options.Applicative
import           GHC.Stats
import           Data.Time.Clock
import           Foreign.Ptr
import           Foreign.Marshal.Utils (copyBytes)
import           Foreign.C.Types
import qualified Data.ByteString.Char8 as B
import qualified Numeric

import System.Posix (closeFd, openFd, defaultFileFlags, OpenMode(ReadOnly))

import           BenchUtils
import           BenchUtils.Types
import qualified Data.Compact as C
import           Data.Compact (Compact)


import qualified CircSim

-- data SerializedCompact a = SerializedCompact {
--   serializedCompactGetBlockList :: [(Ptr a, Word)],
--   serializedCompactGetRoot :: Ptr a
--   }

deriving instance Show (C.SerializedCompact a) 
deriving instance Read (C.SerializedCompact a) 

instance Read (Ptr a) where
  readsPrec _ s =
    case dropWhile isSpace s of
      ('0':'x':rst) ->
        case Numeric.readHex rst of          
          [(ip,rst)] -> [(intPtrToPtr ip,rst)]
          _ -> error $ "Could not read string as hex: "++show s
      _ -> error $ "Could not read string as hex: "++show s

instance NFData CircSim.Boolean where
  rnf CircSim.T = ()
  rnf CircSim.F = ()

--------------------------------------------------------------------------------
-- Main script
--------------------------------------------------------------------------------

dbgPrint _ = return ()
-- dbgPrint = hPutStrLn stderr

main :: IO ()
main = do
  opts@Options{mode,act,tweetMBs} <- execParser cmdOpts

  putStrLn$ "Running with options: "++show opts
  -- Load a gigabyte of data:
--  Just ls <- getConstTwitterData 1024

  rootDir <- getTemporaryDirectory

  let approxLen = tweetMBs * 9091
      approxMiddle = approxLen `quot` 2

  let compactPath = rootDir++"/stored_compacts/twitter_"++show tweetMBs++"MB.compact"
      checkExists = do
         b <- doesCompactPkgExist compactPath
         unless b $ do putStrLn $ "Compacted twitter data doesn't exist, generating: "++compactPath
                       Just ls <- getConstTwitterData tweetMBs
                       putStrLn $ "Got lazy list, now forcing and compacting:"
                       timeit $ evaluate (rnf ls)
                       len <- evaluate (length ls)
                       putStrLn $ "Storing "++show len++" distinct tweets..."
                       cmp <- timeit $ C.compactNewNoShare (10^6)
#ifdef USE_ARRAY                       
                            --  (V.fromList ls)
                              --  As of depth 42823 we get an error on this ^^
                              -- " Invalid non-NFData closure in Compact"
                              (A.listArray (0, len - 1) ls)
                              -- Different error here ^^
                              --   "Claimed but not updated BLACKHOLE in Compact"
#else                              
                              -- Store a tuple so we can access the middle:
--                              (splitAt (len `quot` 2) ls)
                              (splitAt approxMiddle ls)
#endif
                       putStrLn $ "Compaction finished"
                       timeit $ writeCompactPkg compactPath cmp

                       error "TODO: unload from memory so we can reload..."

  ------------------------------------------------------------  
  case (mode,act) of
    (None,BackgroundData) -> timeit$ do
      evaluate $ force $ CircSim.run 8 5000
      return ()

    (Aeson,SearchAll) -> timeit$
                       do Just ls <- getConstTwitterData tweetMBs
                          putStrLn $ "Cats: "++ show (countCats ls)
    (Aeson,ReadRandom) -> timeit$
                       do Just ls <- getConstTwitterData tweetMBs
                          putStrLn $ "Tweet in middle (pos "++ show approxMiddle++"): "
                                      ++ show (ls!!approxMiddle)
    (Aeson,BackgroundData) -> do
      Just ls <- getConstTwitterData tweetMBs
      putStrLn $ "Cats: "++ show (countCats ls)
      performGC
      timeit $ evaluate $ force $ CircSim.run 8 5000
      afterBackgroundData ls

    (Compact,SearchAll) -> do
      checkExists
      timeit $ do 
           cmp <- loadCompactPkg compactPath :: IO (Compact ([TweetMetaData],[TweetMetaData]))
           putStrLn $ "Victory!  Got the compact, type: "++show (typeOf (C.compactGetRoot cmp))
           let tweets = C.compactGetRoot cmp
           putStrLn $ "Cats: "++ show (countCats (fst tweets) + countCats (snd tweets))

    (Compact,ReadRandom) -> do
      checkExists
      timeit$ do
           cmp <- loadCompactPkg compactPath :: IO (Compact ([TweetMetaData],[TweetMetaData]))
           let tweets = C.compactGetRoot cmp
           putStrLn $ "Tweet in middle (pos "++ show approxMiddle++"): "++ show (head $ snd tweets)

    (Compact,BackgroundData) -> do
      checkExists
      cmp <- loadCompactPkg compactPath :: IO (Compact ([TweetMetaData],[TweetMetaData]))
      let tweets = C.compactGetRoot cmp
      putStrLn $ "Cats: "++ show (countCats (fst tweets) + countCats (snd tweets))
      performGC
      timeit $ evaluate $ force $ CircSim.run 8 5000
      afterBackgroundData (fst tweets ++ snd tweets)
--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

{-# NOINLINE afterBackgroundData #-}
afterBackgroundData :: [TweetMetaData] -> IO ()
afterBackgroundData ls =
  putStrLn $ show (length ls)


countCats :: [TweetMetaData] -> Int
countCats ls = loop ls 0
  where
    loop [] !acc = acc
    loop (TweetMetaData{hashtags}:rst) !acc
      | hasCat hashtags = loop rst (acc+1)
      | otherwise       = loop rst acc
    hasCat []        = False
    hasCat (a:rst)
      | a == "cat"   = True
      | otherwise    = hasCat rst

timeit :: IO a -> IO a
timeit act = do
  st <- getCurrentTime
  x <- act
  en <- getCurrentTime
  putStrLn $ "SELFTIMED: "++show (diffUTCTime en st)
  stats <- getGCStats
  putStrLn $ "BYTES_ALLOCATED: "++show (bytesAllocated stats)
  putStrLn $ "GC_WALL_SECONDS: "++show (gcWallSeconds stats)
  return x

----------------------------------------
-- Compact Disk serialization
----------------------------------------

-- A compact package, like a mac-os package, is actually a directory full of files:
doesCompactPkgExist :: FilePath -> IO Bool
doesCompactPkgExist p = do
  -- doesDirectoryExist
  doesFileExist (p </> contentsFile)
  -- TODO, check for proper collection of files in there.

writeCompactPkg :: forall a . (Typeable a, NFData a)
                   => FilePath -> Compact a -> IO ()
writeCompactPkg pth cmp = do
  b <- doesDirectoryExist pth
  when b $ removeDirectoryRecursive pth
  createDirectoryIfMissing True pth
  C.withCompactPtrs cmp $ \ ser@(C.SerializedCompact chunks root) -> do 
    dbgPrint $ " [writeCompactPkgs] Writing out compact to location "
              ++show pth++" with #chunks = "++show (length chunks)

    forM_ chunks $ \ (ptr, len) -> do
      withBinaryFile (chunkpath pth ptr) WriteMode $ \ hnd ->
        hPutBuf hnd ptr (fromIntegral len)
      -- hPutStrLn stderr $ "  ... wrote chunk of len "++ show len

    writeFile (pth </> "type.txt") (show (typeOf (undefined::a)))
    writeFile (pth </> contentsFile) (show ser)
    hPutStrLn stderr $ " [writeCompactPkgs] Succesfully wrote "++(pth </> contentsFile)
  
contentsFile :: FilePath
contentsFile = "contents.txt"

chunkpath :: FilePath -> Ptr a -> String
chunkpath pth ptr = pth </> (show ptr) <.> "bin"

loadCompactPkg :: forall a . NFData a
                  => FilePath -> IO (Compact a)
loadCompactPkg pth = do
  hPutStrLn stderr $ " [loadCompactPkg] Mapping compact into memory from: "++pth
  (ser::C.SerializedCompact a) <- fmap read $ readFile (pth </> contentsFile)
  hPutStrLn stderr $ " [loadCompactPkg] Retrieved SerializedCompact:\n   "++take 100 (show ser)++"..."

  let addrs1  = map fst (C.serializedCompactGetBlockList ser)
      addrSet = S.fromList addrs1

  dbgPrint $ " [loadCompactPkg] Stored SerializedCompact has this many addresses: "++show (length addrs1)

  x <- C.compactImportTrusted ser $ \ dest len -> do
    dbgPrint $ " compactImportTrusted provided addr "++show dest++
      ", is it in the set we expected? "++show (S.member dest addrSet)
    fd <- openFd (chunkpath pth dest) ReadOnly Nothing defaultFileFlags
    dbgPrint $ " Got file descriptor successfully... "
    loc <- c_mmap_at dest (fromIntegral len) (fromIntegral fd)
    let loc2 = castPtr loc
    dbgPrint $ " [loadCompactPkg] performed mmap, got : "++show loc
    when (loc2 /= dest) $ do
       hPutStrLn stderr $ " [loadCompactPkg] File landed in memory, but not in the right place.. memcpy time."
       copyBytes dest loc2 (fromIntegral len)
       hPutStrLn stderr $ " [loadCompactPkg] Done memcpy"
       c_munmap loc (fromIntegral len)
       hPutStrLn stderr $ " [loadCompactPkg] out-of-place region unmapped"

    closeFd fd -- Ok to close, mapping persists.
  case x of
    Just c -> return c

foreign import ccall unsafe "hs_mmap_at.h hs_mmap_at"
    c_mmap_at   :: Ptr a -> CSize -> CInt -> IO (Ptr Word8)

foreign import ccall unsafe "hs_bytestring_mmap.h munmap"
    c_munmap :: Ptr Word8 -> CSize -> IO CInt

--------------------------------------------------------------------------------
--  Arg parsing:
--------------------------------------------------------------------------------


data Mode     = Aeson
              | Compact
              | None deriving (Show,Read,Eq,Ord,Enum,Bounded)
 
data Action   = -- LoadAll     -- ^ load all tweets in memory and stop.
                ReadRandom  -- ^ Read one tweet from the middle
                -- { howMany :: Int } -- ^ read N tweets at (uniform) random
              | SearchAll   -- ^ read all tweets and count a hash tag
              | BackgroundData -- ^ load tweets in memory and go on to do something else
  deriving (Show,Read,Eq,Ord)

data Options = Options
               { mode       :: Mode
               , act        :: Action
               , tweetMBs   :: Int
                 -- ^ Powers of two up to 1024...
               }
  deriving (Show,Read,Eq,Ord)

cmdOpts :: ParserInfo Options
cmdOpts = info (helper <*> argParser)
          ( fullDesc
            <> progDesc ("Run data loading benchmark.")
          )

argParser :: Parser Options
argParser = Options <$>
--  argument auto (help "can be 'Lists' or 'Sets'" <> metavar "<datatype>") <*>
  option  auto (long "mode" <> short 'm' <>
                metavar "<mode>" <>
                help ("'Aeson' or 'Compact'")) <*>
  option auto (long "action" <> short 'a' <>
               metavar "<act>" <>
               help "can be LoadAll, SeachAll, or 'ReadRandom <N>'") <*>  
  -- argument auto (help ("'Aeson' or 'Compact'") <> metavar "<mode>") <*>
  -- argument auto (help "can be LoadAll, SeachAll, or 'ReadRandom <N>'" <> metavar "<act>") <*>  
  argument auto (help "number of MBs of tweets 2^0..2^10" <> metavar "<numTweetMB>") 
