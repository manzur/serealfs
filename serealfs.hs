
import Prelude hiding(concat, lookup, keys, take, drop, byteSubstring)
import Data.Either
import Data.List (isSuffixOf)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy  as LazyString
import qualified Data.ByteString.Char8 as Char8

import qualified Data.Map.Lazy as Map

import System.IO (stdin, stdout, hSetBuffering, BufferMode(..))
import System.Directory (getDirectoryContents)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.Fuse
import System.Posix.Types

import Sereal.Decoder 
import Sereal.Traversable (lookup, keys)

import SerealFS.Utils (
    unionFileModes',
    splitPath,
    calculateFileSize,
    buildDefaultFileStat,
    lookupFileEntry,
    getFileStatForEntry,
    getFileStatForPath,
    serealfs_default_mode,
    byteSubstring,
    rootDirStat )


createFuseBindings :: SerealBody -> FilePath -> FuseOperations SerealBody
createFuseBindings rootSereal rootDirectory = 
    defaultFuseOps {

        fuseGetFileSystemStats = srlFileSystemStat,
        fuseGetFileStat        = srlGetFileStat rootSereal,

        fuseOpen               = srlFileOpen rootSereal,
        fuseRead               = srlFileRead rootSereal,

        fuseOpenDirectory      = srlOpenDirectory,
        fuseReadDirectory      = srlReadDirectory rootSereal,

        -- dummy functions that just return ePERM
        fuseCreateDevice       = srlCreateDevice,
        fuseCreateDirectory    = srlCreateDirectory,
        fuseCreateLink         = srlCreateLink,
        fuseCreateSymbolicLink = srlCreateSymbolicLink,
        fuseRemoveLink         = srlRemoveLink,
        fuseRemoveDirectory    = srlRemoveDirectory,
        fuseAccess             = srlFileAccess,
        fuseSetFileMode        = srlSetFileMode,
        fuseSetFileTimes       = srlSetFileTimes,
        fuseSetOwnerAndGroup   = srlSetOwnerAndGroup
    }

-- NOTE: returning number of files, blocks does not have any sense here,
-- so let's just return zero values for them
srlFileSystemStat :: String -> IO (Either Errno FileSystemStats) 
srlFileSystemStat rootDirectory = 
    return $ 
        Right FileSystemStats {
            fsStatBlockSize       = 0,
            fsStatBlockCount      = 0,
            fsStatBlocksFree      = 0,
            fsStatBlocksAvailable = 0,
            fsStatFileCount       = 0,
            fsStatFilesFree       = 0,
            fsStatMaxNameLength   = 0
        }


srlGetFileStat :: SerealBody -> (FilePath -> IO (Either Errno FileStat))
srlGetFileStat rootSereal = \filePath -> do

    if filePath == "/" 
        then
            fmap Right rootDirStat
        else do
            let pathItems  = splitPath filePath
            let maybeEntry = lookupFileEntry rootSereal pathItems

            case maybeEntry of 
                Just entry -> do
                    fmap Right $ getFileStatForEntry entry

                Nothing -> do
                    return $ Left eNOENT
    

srlCreateSymbolicLink :: FilePath -> FilePath -> IO Errno
srlCreateSymbolicLink filePath _ = return ePERM 

srlCreateLink :: FilePath -> FilePath -> IO Errno
srlCreateLink srcLink dstLink = return ePERM

fsRename :: FilePath -> FilePath -> IO Errno
fsRename srcFile dstFile = return ePERM

srlCreateDevice :: FilePath -> EntryType -> FileMode -> DeviceID -> IO Errno
srlCreateDevice filePath _ _ _ = return ePERM

srlCreateDirectory :: FilePath -> FileMode -> IO Errno
srlCreateDirectory filePath _ = return ePERM

srlOpenDirectory :: FilePath -> IO Errno
srlOpenDirectory directory = return eOK

srlFileOpen :: SerealBody -> (FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno SerealBody))
srlFileOpen rootSereal = \filePath openMode openFileFlags -> do

    case openMode of 
        WriteOnly -> return $ Left ePERM
        ReadWrite -> return $ Left ePERM

        otherwise -> do
            let pathItems  = splitPath filePath
            let maybeEntry = lookupFileEntry rootSereal pathItems

            case maybeEntry of
                Just entry -> return $ Right entry
                _          -> return $ Left eNOENT


srlFileRead :: SerealBody -> (FilePath -> SerealBody -> ByteCount -> FileOffset -> IO (Either Errno ByteString))
srlFileRead rootSereal = \filePath serealEntry length offset ->
    return $ case serealEntry of
            SrlUndef         -> Right $ Char8.pack ""
            SrlString  value -> Right $ byteSubstring value offset length
            SrlInteger value -> Right $ Char8.pack $ show value
            SrlFloat   value -> Right $ Char8.pack $ show value
            SrlDouble  value -> Right $ Char8.pack $ show value
            SrlBool    value -> Right $ Char8.pack $ show value
            _                -> Left eNOENT

srlSetFileMode :: FilePath -> FileMode -> IO Errno
srlSetFileMode filePath _ = return ePERM

srlSetFileTimes :: FilePath -> EpochTime -> EpochTime -> IO Errno
srlSetFileTimes filePath _ _ = return ePERM

srlSetOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO Errno
srlSetOwnerAndGroup filePath _ _ = return ePERM

srlRemoveLink :: FilePath -> IO Errno
srlRemoveLink filePath = return ePERM

srlRemoveDirectory :: FilePath -> IO Errno
srlRemoveDirectory filePath = return ePERM

getFileStatForEntries :: SerealBody -> [FilePath] -> IO [(FilePath, FileStat)]
getFileStatForEntries rootSereal filePaths = do
    sequence 
        $ map (\filepath -> do
                fileStat <- getFileStatForPath rootSereal filepath
                let filename = last $ splitPath filepath
                return $ (filename, fileStat)
              )
              filePaths 

srlReadDirectory :: SerealBody -> (FilePath -> IO (Either Errno [(FilePath, FileStat)]))
srlReadDirectory rootSereal = \directory -> do

    let filenames = if directory == "/" 
                        then keys rootSereal
                        else do
                            let pathItems = splitPath directory
                            case (lookupFileEntry rootSereal pathItems) of
                                Just entry -> keys entry
                                Nothing    -> []

    let dirPath = directory ++ "/"
    let fullPathEntries = map ((++) dirPath) filenames

    fmap Right $ getFileStatForEntries rootSereal fullPathEntries

srlFileAccess :: FilePath -> Int -> IO Errno
srlFileAccess a b = return eOK

constructSereal :: (FilePath, LazyString.ByteString) -> SerealBody -> SerealBody
constructSereal  (filename, inputByte) acc@(SrlHash entries) =
    case decodeSereal inputByte of
        Just (_, serealBody) -> SrlHash $ Map.insert filename serealBody entries
        Nothing -> acc

mountDirectory :: String -> IO ()
mountDirectory directory = do
    files <- getDirectoryContents directory
    let serealFiles = filter (isSuffixOf ".srl") files

    let dirpath = directory ++ "/"

    filesContents <- sequence $ map (LazyString.readFile . (++) dirpath) serealFiles

    let emptySerealBody = SrlHash $ Map.fromList []

    let filesAndContents = zip serealFiles filesContents

    let rootSerealBody = foldr constructSereal emptySerealBody filesAndContents

    --- Launch the fuse with the callback ---
    let fuseBindings = createFuseBindings rootSerealBody directory
    fuseMain fuseBindings defaultExceptionHandler

main :: IO ()
main = do
    --- Initial setup ---

    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering

    --- Parsing arguments and sereal file ---

    args <- getArgs

    case args of 
        (rootDirectory : fuseOptions) -> mountDirectory rootDirectory
        _ -> putStrLn "Wrong number of arguments: serealfs <directory-with-sereal-files>"  >> exitFailure

