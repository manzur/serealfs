module Sereal.Decoder (
    SerealBody(..), 
    SerealHeader(..), 
    SerealHashKey,
    decodeSereal
) where

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754

import Safe (readMay)

import qualified Data.ByteString.Lazy  as LazyString
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Map.Lazy         as Map
import qualified Data.ByteString.UTF8  as Utf8Byte
import qualified Data.List             as List

import Sereal.Types
import Sereal.Traversable

instance Traversable SerealBody where
    lookup = serealBodyLookup
    keys   = serealBodyKeys

parseHeader :: Get SerealHeader
parseHeader = do
    magic   <- getWord32le
    version <- getWord8
    suffix  <- getWord8
    return $ SerealHeader magic version suffix

validHeader :: SerealHeader -> Bool
validHeader (SerealHeader magic version suffix) = 
     magic == srl_version3_magic 
  && version == 3


parseBinaryString :: Word8 -> Get String
parseBinaryString tag = fmap Char8.unpack $ getByteString length
    where length = fromIntegral (tag - srl_tag_short_binary_0)

parseHashKey :: Get SerealHashKey
parseHashKey = do
    tag <- getWord8

    if tag >= srl_tag_short_binary_0 && tag <= srl_tag_short_binary_31 
        then
            parseBinaryString tag
        else error "Only short binary hash keys are allowed"

parseHashValue :: Get SerealBody
parseHashValue = do
    tag   <- getWord8
    value <- parse tag
    return value

readPair :: Get (SerealHashKey, SerealBody)
readPair = do
    key   <- parseHashKey
    value <- parseHashValue
    return $ (key, value)

readHashEntries :: Get [(SerealHashKey, SerealBody)] -> Int -> Get [(SerealHashKey, SerealBody)]
readHashEntries acc _ = do
    rest <- acc
    item <- readPair
    return $ item : rest
    
parseHashEntries :: Int -> Get [(SerealHashKey, SerealBody)]
parseHashEntries n = foldl readHashEntries initialData [1..n]
    where initialData = return []

parseHash :: Int -> Get SerealBody
parseHash size = fmap (SrlHash . Map.fromList) $ parseHashEntries size

readArrayEntries :: Get [SerealBody] -> Int -> Get [SerealBody]
readArrayEntries acc _ = do
    rest <- acc
    item <- parseBody
    return $ rest ++ [item]
 
parseArray :: Int -> Get [SerealBody]
parseArray size = foldl readArrayEntries initialData [1..size]
    where initialData = return []
      
parseBody :: Get SerealBody
parseBody = do
    tag <- getWord8
    parse tag

readVarInt :: Get [Int]
readVarInt = do
    v8 <- getWord8
    let value = fromIntegral v8 :: Int

    if testBit value 7
        then do
            rest <- readVarInt
            return $ value : rest
        else return $ [value]

parseVarInt :: Get Integer
parseVarInt = do
    varint <- readVarInt
    return $ fromIntegral 
           $ foldl (\acc v-> (v + shiftL acc 7)) 0 
           $ reverse 
           $ map (flip clearBit 7) varint

parse :: Word8 -> Get SerealBody
-- pos_0..15
parse tag | tag >= srl_tag_pos_0 && tag <= srl_tag_pos_15 = return $ SrlInteger $ fromIntegral(tag)

-- neg 16..1
parse tag | tag >= srl_tag_neg_16 && tag <= srl_tag_neg_1 = return $ SrlInteger $ -32 + fromIntegral(tag)

-- varint 
parse tag | tag == srl_tag_varint = do
    varInt <- parseVarInt
    return $ SrlInteger varInt

-- undef
parse tag | tag == srl_tag_undef || tag == srl_tag_canon_undef = return $ SrlUndef

-- binary 
parse tag | tag == srl_tag_binary = do
    length <- parseVarInt
    result <- getByteString (fromIntegral length)
    return $ SrlString $ Char8.unpack result

-- utf8 
parse tag | tag == srl_tag_utf8 = do
    length <- parseVarInt
    result <- getByteString (fromIntegral length)
    return $ SrlString $ Utf8Byte.toString result

-- false/true 
parse tag | tag == srl_tag_false = return $ SrlBool False
parse tag | tag == srl_tag_true  = return $ SrlBool True

-- Float
parse tag | tag == srl_tag_float = do
    value <- getFloat32le
    return $ SrlFloat value

-- Double
parse tag | tag == srl_tag_double = do
    value <- getFloat64le
    return $ SrlDouble value

-- hashref_0..15
parse tag | tag == srl_tag_hashref_0 = return $ SrlHash Map.empty
parse tag | tag > srl_tag_hashref_0 && tag <= srl_tag_hashref_15 = parseHash size
    where size = fromIntegral (tag - srl_tag_hashref_0)

-- ref
parse tag | tag == srl_tag_refn = do
    next <- getWord8
    parse next

parse tag | tag == srl_tag_hash = do
    size <- parseVarInt
    parseHash $ fromIntegral size

parse tag | tag == srl_tag_array = do
    size <- parseVarInt
    result <- parseArray $ fromIntegral size
    return $ SrlArray result

-- arrayref..15
parse tag | tag == srl_tag_arrayref_0 = return $ SrlArray []
parse tag | tag > srl_tag_arrayref_0 && tag <= srl_tag_arrayref_15 = do
    let size = fromIntegral (tag - srl_tag_arrayref_0)
    result <- parseArray size
    return $ SrlArray result

-- short_binary_0..31 
parse tag | tag >= srl_tag_short_binary_0 && tag <= srl_tag_short_binary_31 = fmap (SrlString . Char8.unpack) $ getByteString length
    where length = fromIntegral (tag - srl_tag_short_binary_0)

parse tag | tag == srl_tag_zigzag = fmap (SrlInteger . convertZigZag) parseVarInt
    where convertZigZag v = (0 - (v + 1) `div` 2)

parse tag = error $ "Unimplemented tag: " ++ show tag

decodeSereal :: LazyString.ByteString -> Maybe (SerealHeader, SerealBody)
decodeSereal contents = do
    let header = runGet parseHeader contents
    let rest   = LazyString.drop srl_header_length contents 

    if validHeader header 
        then 
            let body = runGet parseBody rest in
                Just $ (header, body)
        else Nothing

serealBodyLookup :: SerealHashKey -> SerealBody -> Maybe SerealBody
serealBodyLookup key (SrlHash map) = Map.lookup key map
serealBodyLookup key (SrlArray array) = do
    index <- readMay key
    if index < 0 || index >= length array 
        then Nothing
        else Just $ array !! index

serealBodyKeys :: SerealBody -> [SerealHashKey]
serealBodyKeys (SrlHash map)    = Map.keys map
serealBodyKeys (SrlArray array) = map show [0..length array  - 1]


