module SerealFS.Utils (
    serealfs_default_mode,
    splitPath,
    unionFileModes',
    calculateFileSize,
    rootDirStat,
    buildDefaultDirStat,
    buildDefaultFileStat,
    lookupFileEntry,
    getFileStatForEntry,
    getFileStatForPath,
    defaultKeyStat,
    byteSubstring
) where

import Prelude hiding(lookup)

import Data.ByteString (ByteString)
import Data.List.Split (splitOn)
import qualified Data.ByteString.Char8 as Char8 

import Foreign.C.Types (CSize)
import System.Posix.Types (FileMode, COff)
import System.Posix.Files (unionFileModes, directoryMode, ownerReadMode, groupReadMode, otherReadMode)
import System.Posix.User (getEffectiveUserID, getEffectiveGroupID)
import System.Posix.Time (epochTime)

import System.Fuse

import Sereal.Types
import Sereal.Traversable (lookup, keys)
import Sereal.Decoder

serealfs_default_mode = unionFileModes' [ownerReadMode, groupReadMode, otherReadMode]

unionFileModes' :: [FileMode] -> FileMode
unionFileModes' (fileMode:[]) = fileMode
unionFileModes' (f : r)       = foldl unionFileModes f r

splitPath :: String -> [String]
splitPath filePath = 
    [ item | item <- splitOn "/" filePath, length item > 0 ]


calculateFileSize :: SerealBody -> COff
calculateFileSize (SrlString entry)  = fromIntegral $ Char8.length $ Char8.pack entry
calculateFileSize (SrlHash _)        = fromIntegral $ 4096
calculateFileSize (SrlArray _)       = fromIntegral $ 4096
calculateFileSize (SrlInteger entry) = fromIntegral $ Char8.length $ Char8.pack $ show entry
calculateFileSize (SrlBool entry)    = fromIntegral $ Char8.length $ Char8.pack $ show entry
calculateFileSize (SrlFloat entry)   = fromIntegral $ Char8.length $ Char8.pack $ show entry
calculateFileSize (SrlDouble entry)  = fromIntegral $ Char8.length $ Char8.pack $ show entry
calculateFileSize (SrlUndef )        = fromIntegral 0

defaultKeyStat = FileStat {
    statEntryType = RegularFile,
    statFileMode  = serealfs_default_mode,
    statLinkCount = 1,
    statFileOwner = 0,
    statFileGroup = 0,
    statSpecialDeviceID = 0,
    statFileSize = 0,
    statBlocks = 0,
    statAccessTime = 200000,
    statModificationTime = 200000,
    statStatusChangeTime = 200000
}

rootDirStat = do
    userId    <- getEffectiveUserID
    groupId   <- getEffectiveGroupID
    mountTime <- epochTime

    return $ FileStat {
        statEntryType = Directory,
        statFileMode  = unionFileModes' [directoryMode, serealfs_default_mode],
        statLinkCount = 1,
        statFileOwner = userId,
        statFileGroup = groupId,
        statSpecialDeviceID = 0,
        statFileSize = 0,
        statBlocks = 0,
        statAccessTime = mountTime,
        statModificationTime = mountTime,
        statStatusChangeTime = mountTime
    }

buildDefaultFileStat = do
    userID    <- getEffectiveUserID
    groupID   <- getEffectiveGroupID
    mountTime <- epochTime
 
    return $ FileStat {
        statEntryType = RegularFile,
        statFileMode  = serealfs_default_mode,
        statLinkCount = 1,
        statFileOwner = userID,
        statFileGroup = groupID,
        statSpecialDeviceID = 0,
        statFileSize = 0,
        statBlocks = 0,
        statAccessTime = mountTime,
        statModificationTime = mountTime,
        statStatusChangeTime = mountTime
    }

buildDefaultDirStat = do
    result <- buildDefaultFileStat 
    return $ result { statEntryType = Directory, statFileSize = 4096 }

getFileStatForEntry :: SerealBody -> IO FileStat
getFileStatForEntry   entry@(SrlHash _)   = do
    defaultDirStat <- buildDefaultDirStat
    return $ defaultDirStat { statFileSize = calculateFileSize entry }

getFileStatForEntry   entry@(SrlArray _)  = do
    defaultDirStat <- buildDefaultDirStat
    return $ defaultDirStat { statFileSize = calculateFileSize entry }

getFileStatForEntry   entry               = do 
    defaultFileStat <- buildDefaultFileStat
    return $ defaultFileStat {statFileSize = calculateFileSize entry }

getFileStatForPath :: SerealBody -> String -> IO FileStat
getFileStatForPath rootSereal filePath = do
    defaultFileStat <- buildDefaultFileStat

    let pathItems  = splitPath filePath
    let maybeEntry = lookupFileEntry rootSereal pathItems
    
    case maybeEntry of 
        Just e  ->  getFileStatForEntry e
        Nothing ->  return $ defaultFileStat

lookupFileEntry :: SerealBody -> [SerealHashKey] -> Maybe SerealBody
lookupFileEntry fsMap [filePath]   = lookup filePath fsMap
lookupFileEntry fsMap (filePath:rest) = case lookup filePath fsMap of
                                         Just serealBody -> lookupFileEntry serealBody rest
                                         _ -> Nothing

byteSubstring :: String -> COff -> CSize -> ByteString
byteSubstring value offset length =
     Char8.take length' $ Char8.drop offset' $ Char8.pack value

     where length' = fromIntegral length
           offset' = fromIntegral offset
