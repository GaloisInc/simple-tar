{-# LANGUAGE RecordWildCards #-}
module Codec.Archive.Tar(
         Archive
       , ArchiveHeader(..)
       , ArchiveMember(..)
       , RegularFile(..)
       , Link(..)
       , Device(..)
       , SpecialFIFOFile(..)
       , unarchive
       )
 where

import           Control.Monad(unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Char(digitToInt)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Serialize.Get
import           Data.String(IsString(..))
import           Data.Time.Clock(UTCTime(..))
import           Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import           Data.Time.LocalTime()
import           Data.ByteString(ByteString)
import           Data.Word(Word)

type Archive = Map FilePath ArchiveMember

data ArchiveMember = RegularFileMember     RegularFile
                   | LinkMember            Link
                   | SymbolicLinkMember    Link
                   | CharacterDeviceMember Device
                   | BlockDeviceMember     Device
                   | DirectoryMember       Directory
                   | SpecialFIFOFileMember SpecialFIFOFile
                   | ReservedMember        RegularFile
                   | CustomMember          Char ArchiveHeader ByteString
 deriving (Show)

unarchive :: ByteString -> Either String Archive
unarchive = runGet getMembers

data ArchiveHeader = ArchiveHeader
     { hdrFileName         :: FilePath
     , hdrMode             :: Word
     , hdrOwner            :: Word
     , hdrGroup            :: Word
     , hdrSize             :: Word
     , hdrModificationTime :: UTCTime
     , hdrChecksum         :: Word
     , hdrTypeFlag         :: RecordTypeFlag
     , hdrLinkName         :: FilePath
     , hdrMagic            :: String
     , hdrVersion          :: ByteString
     , hdrUserName         :: String
     , hdrGroupName        :: String
     , hdrDeviceMajor      :: Word
     , hdrDeviceMinor      :: Word
     , hdrPrefix           :: FilePath
     }
 deriving (Show)

getMembers :: Get Archive
getMembers =
  do next <- nextMember
     case next of
       Nothing           -> return Map.empty
       Just (key, value) -> Map.insert key value `fmap` getMembers

nextMember :: Get (Maybe (FilePath, ArchiveMember))
nextMember =
  do nextTwoBlocks <- lookAhead (getBytes 1024)
     if B.all (== 0) nextTwoBlocks
        then return Nothing
        else Just `fmap` getArchiveMember

getArchiveMember :: Get (FilePath, ArchiveMember)
getArchiveMember =
  do hdr <- isolate 512 getProperHeader
     case hdrTypeFlag hdr of
       FlagRegularFile     -> convertRegularFile                       hdr
       FlagLink            -> convertLink        LinkMember            hdr
       FlagSymbolicLink    -> convertLink        SymbolicLinkMember    hdr
       FlagCharacterDevice -> convertDevice      CharacterDeviceMember hdr
       FlagBlockDevice     -> convertDevice      BlockDeviceMember     hdr
       FlagDirectory       -> convertDirectory                         hdr
       FlagSpecialFIFOFile -> convertFIFO                              hdr
       FlagReserved        -> convertRegularFile                       hdr
       FlagCustom c        -> do body <- getPaddedBody (hdrSize hdr)
                                 let key = hdrPrefix hdr ++ hdrFileName hdr
                                 return (key, CustomMember c hdr body)

data RegularFile = RegularFile
     { regFileName         :: FilePath
     , regMode             :: Word
     , regOwner            :: Word
     , regGroup            :: Word
     , regSize             :: Word
     , regModificationTime :: UTCTime
     , regUserName         :: String
     , regGroupName        :: String
     , regContents         :: ByteString
     }
 deriving (Show)

convertRegularFile :: ArchiveHeader -> Get (FilePath, ArchiveMember)
convertRegularFile hdr =
  do let regFileName         = hdrPrefix           hdr ++ hdrFileName hdr
         regMode             = hdrMode             hdr
         regOwner            = hdrOwner            hdr
         regGroup            = hdrGroup            hdr
         regSize             = hdrSize             hdr
         regModificationTime = hdrModificationTime hdr
         regUserName         = hdrUserName         hdr
         regGroupName        = hdrGroupName        hdr
     regContents <- getPaddedBody regSize
     return (regFileName, RegularFileMember RegularFile{..})

data Link = Link
     { linkFileName         :: FilePath
     , linkMode             :: Word
     , linkOwner            :: Word
     , linkGroup            :: Word
     , linkSize             :: Word
     , linkModificationTime :: UTCTime
     , linkUserName         :: String
     , linkGroupName        :: String
     , linkTarget           :: FilePath
     }
 deriving (Show)

convertLink :: (Link -> ArchiveMember) -> ArchiveHeader ->
               Get (FilePath, ArchiveMember)
convertLink builder hdr = return (linkFileName, builder link)
 where
  link                 = Link{ .. }
  linkFileName         = hdrPrefix           hdr ++ hdrFileName hdr
  linkMode             = hdrMode             hdr
  linkOwner            = hdrOwner            hdr
  linkGroup            = hdrGroup            hdr
  linkSize             = hdrSize             hdr
  linkModificationTime = hdrModificationTime hdr
  linkUserName         = hdrUserName         hdr
  linkGroupName        = hdrGroupName        hdr
  linkTarget           = hdrLinkName         hdr

data Device = Device
     { devFileName         :: FilePath
     , devMode             :: Word
     , devOwner            :: Word
     , devGroup            :: Word
     , devModificationTime :: UTCTime
     , devUserName         :: String
     , devGroupName        :: String
     , devMajorNumber      :: Word
     , devMinorNumber      :: Word
     }
 deriving (Show)
 
convertDevice :: (Device -> ArchiveMember) -> ArchiveHeader ->
                 Get (FilePath, ArchiveMember)
convertDevice builder hdr = return (devFileName, builder device)
 where
  device              = Device{..}
  devFileName         = hdrPrefix           hdr ++ hdrFileName hdr
  devMode             = hdrMode             hdr
  devOwner            = hdrOwner            hdr
  devGroup            = hdrGroup            hdr
  devModificationTime = hdrModificationTime hdr
  devUserName         = hdrUserName         hdr
  devGroupName        = hdrGroupName        hdr
  devMajorNumber      = hdrDeviceMajor      hdr
  devMinorNumber      = hdrDeviceMinor      hdr

data Directory = Directory
     { dirFileName         :: FilePath
     , dirMode             :: Word
     , dirOwner            :: Word
     , dirGroup            :: Word
     , dirModificationTime :: UTCTime
     , dirUserName         :: String
     , dirGroupName        :: String
     }
 deriving (Show)

convertDirectory :: ArchiveHeader -> Get (FilePath, ArchiveMember)
convertDirectory hdr = return (dirFileName, DirectoryMember directory)
 where
  directory           = Directory{..}
  dirFileName         = hdrPrefix           hdr ++ hdrFileName hdr
  dirMode             = hdrMode             hdr
  dirOwner            = hdrOwner            hdr
  dirGroup            = hdrGroup            hdr
  dirModificationTime = hdrModificationTime hdr
  dirUserName         = hdrUserName         hdr
  dirGroupName        = hdrGroupName        hdr

data SpecialFIFOFile = SpecialFIFOFile
     { fifoFileName         :: FilePath
     , fifoMode             :: Word
     , fifoOwner            :: Word
     , fifoGroup            :: Word
     , fifoModificationTime :: UTCTime
     , fifoUserName         :: String
     , fifoGroupName        :: String
     }
 deriving (Show)

convertFIFO :: ArchiveHeader -> Get (FilePath, ArchiveMember)
convertFIFO hdr = return (fifoFileName, SpecialFIFOFileMember fifo)
 where
  fifo                 = SpecialFIFOFile{..}
  fifoFileName         = hdrPrefix           hdr ++ hdrFileName hdr
  fifoMode             = hdrMode             hdr
  fifoOwner            = hdrOwner            hdr
  fifoGroup            = hdrGroup            hdr
  fifoModificationTime = hdrModificationTime hdr
  fifoUserName         = hdrUserName         hdr
  fifoGroupName        = hdrGroupName        hdr


getPaddedBody :: Word -> Get ByteString
getPaddedBody sizeb =
  do let blocks  = (sizeb + 511) `div` 512
         padded  = blocks * 512
         skipAmt = padded - sizeb
     res <- getByteString (fromIntegral sizeb)
     unless (skipAmt == 0) $ skip (fromIntegral skipAmt)
     return res

getProperHeader :: Get ArchiveHeader
getProperHeader =
  do checksum1 <- lookAhead (label "checksum computation" getChecksum)
     header    <- label "archive header" getArchiveHeader
     let checksum2 = hdrChecksum header
     unless (checksum1 == checksum2) $
       fail ("Checksum mismatch: "++show checksum1++" /= "++show checksum2)
     return header

getChecksum :: Get Word
getChecksum =
  do acc0  <- addSum 0     `fmap` getByteString 100 -- name of file
     acc1  <- addSum acc0  `fmap` getByteString 8   -- file mode
     acc2  <- addSum acc1  `fmap` getByteString 8   -- user ID
     acc3  <- addSum acc2  `fmap` getByteString 8   -- group ID
     acc4  <- addSum acc3  `fmap` getByteString 12  -- length in bytes
     acc5  <- addSum acc4  `fmap` getByteString 12  -- modify time
     skip 8 -- the actual checksum
     let acc6 = addSum acc5 (BC.replicate 8 ' ')
     acc7  <- addSum acc6  `fmap` getByteString 1   -- type of file
     acc8  <- addSum acc7  `fmap` getByteString 100 -- name of linked file
     acc9  <- addSum acc8  `fmap` getByteString 6   -- USTAR indicator
     acc10 <- addSum acc9  `fmap` getByteString 2   -- USTAR version
     acc11 <- addSum acc10 `fmap` getByteString 32  -- user name
     acc12 <- addSum acc11 `fmap` getByteString 32  -- user group
     acc13 <- addSum acc12 `fmap` getByteString 8   -- device major number
     acc14 <- addSum acc13 `fmap` getByteString 8   -- device minor number
     acc15 <- addSum acc14 `fmap` getByteString 155 -- prefix file name
     skip 12 -- trailing bytes
     return acc15
 where
  addSum = B.foldl' (\ acc x -> acc + fromIntegral x)
 
getArchiveHeader :: Get ArchiveHeader
getArchiveHeader =
  do hdrFileName         <- toStringLike    `fmap` getByteString 100
     hdrMode             <- toNumeric       `fmap` getByteString 8
     hdrOwner            <- toNumeric       `fmap` getByteString 8
     hdrGroup            <- toNumeric       `fmap` getByteString 8
     hdrSize             <- toNumeric       `fmap` getByteString 12
     hdrModificationTime <- toUTCTime       `fmap` getByteString 12
     hdrChecksum         <- toNumeric       `fmap` getByteString 8
     hdrTypeFlag         <- toRecordTypeFlag =<<   getByteString 1
     hdrLinkName         <- toStringLike    `fmap` getByteString 100
     hdrMagic            <- toStringLike    `fmap` getByteString 6
     hdrVersion          <-                        getByteString 2
     hdrUserName         <- toStringLike    `fmap` getByteString 32
     hdrGroupName        <- toStringLike    `fmap` getByteString 32
     hdrDeviceMajor      <- toNumeric       `fmap` getByteString 8
     hdrDeviceMinor      <- toNumeric       `fmap` getByteString 8
     hdrPrefix           <- toStringLike    `fmap` getByteString 155
     skip 12 -- trailing bytes
     unless (hdrMagic == "ustar ") $ fail ("Bad magic value: " ++ hdrMagic ++ "(filename: " ++ show hdrFileName)
     return ArchiveHeader{..}

data RecordTypeFlag = FlagRegularFile
                    | FlagLink
                    | FlagSymbolicLink
                    | FlagCharacterDevice
                    | FlagBlockDevice
                    | FlagDirectory
                    | FlagSpecialFIFOFile
                    | FlagReserved
                    | FlagCustom Char
 deriving (Show)

toRecordTypeFlag :: ByteString -> Get RecordTypeFlag
toRecordTypeFlag x =
  case BC.uncons x of
    Nothing   -> fail "The world went insane."
    Just ('\0', _) -> return FlagRegularFile
    Just ('0',  _) -> return FlagRegularFile
    Just ('1',  _) -> return FlagLink
    Just ('2',  _) -> return FlagSymbolicLink
    Just ('3',  _) -> return FlagCharacterDevice
    Just ('4',  _) -> return FlagBlockDevice
    Just ('5',  _) -> return FlagDirectory
    Just ('6',  _) -> return FlagSpecialFIFOFile
    Just ('7',  _) -> return FlagReserved
    Just (c,    _) | c `elem` (['A'..'Z']++['a'..'z']) ->
      return (FlagCustom c)
    Just _ ->
      fail ("Unexpected record type: " ++ show x)

toStringLike :: IsString a => ByteString -> a
toStringLike = fromString . BC.unpack . stripTrailingNulls
 where
  stripTrailingNulls x =
    case B.unsnoc x of
      Nothing      -> B.empty
      Just (x', 0) -> stripTrailingNulls x'
      Just (_,  _) -> x

toNumeric :: Num a => ByteString -> a
toNumeric = parseOctal 0
 where
  parseOctal acc x =
    case BC.uncons x of
      Nothing        -> acc
      Just ('\0', _) -> acc
      Just (c, rest) -> parseOctal ((acc * 8) + digitToInt' c) rest
  digitToInt' = fromIntegral . digitToInt

toUTCTime :: ByteString -> UTCTime
toUTCTime = posixSecondsToUTCTime . toNumeric
