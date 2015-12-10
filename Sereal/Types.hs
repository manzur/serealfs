module Sereal.Types where

import Data.Int
import Data.Word

import qualified Data.Map as Map

srl_version3_magic  = 0x6C72F33D :: Word32
srl_header_length   = 6 :: Int64

srl_tag_pos_0           = 0x0  :: Word8
srl_tag_pos_15          = 0xf  :: Word8
srl_tag_neg_16          = 0x10 :: Word8
srl_tag_neg_1           = 0x1f :: Word8
srl_tag_varint          = 0x20 :: Word8
srl_tag_zigzag          = 0x21 :: Word8
srl_tag_float           = 0x22 :: Word8
srl_tag_double          = 0x23 :: Word8
srl_tag_undef           = 0x25 :: Word8
srl_tag_binary          = 0x26 :: Word8
srl_tag_utf8            = 0x27 :: Word8
srl_tag_refn            = 0x28 :: Word8
srl_tag_hash            = 0x2a :: Word8
srl_tag_array           = 0x2b :: Word8
srl_tag_canon_undef     = 0x39 :: Word8
srl_tag_false           = 0x3a :: Word8
srl_tag_true            = 0x3b :: Word8
srl_tag_arrayref_0      = 0x40 :: Word8
srl_tag_arrayref_15     = 0x4f :: Word8
srl_tag_hashref_0       = 0x50 :: Word8
srl_tag_hashref_15      = 0x5f :: Word8
srl_tag_short_binary_0  = 0x60 :: Word8
srl_tag_short_binary_31 = 0x7f :: Word8

data SerealHeader = SerealHeader {
    magic            :: !Word32,
    version_protocol :: !Word8,
    suffix_length    :: !Word8
} deriving (Show)

-- currently only String(which is actually any Stringifiable) and Hash supported
type SerealHashKey = String

data SerealBody = SrlString  String
                | SrlBool    Bool
                | SrlUndef
                | SrlInteger Integer
                | SrlFloat   Float
                | SrlDouble  Double
                | SrlHash (Map.Map SerealHashKey SerealBody)
                | SrlArray  [SerealBody] -- just list for now
    deriving(Show)

