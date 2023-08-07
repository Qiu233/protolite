{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ProtoLite.Encode where

import ProtoLite.Types
import Data.Int
import Data.Bits
import qualified Data.ByteString.Lazy as B
import GHC.Float (castFloatToWord32, castDoubleToWord64)
import Data.Word
import Data.Binary
import Data.Binary.Put

putTag :: PTag -> Put
putTag (PTag _fieldNumber _wireType) = do
    let v = PVInt $ (fromIntegral _fieldNumber `shiftL` 3) .|. fromIntegral _wireType
    putPVInt v

putPVIntIndex :: Word32 -> PVInt -> PMessageEntry
putPVIntIndex field value = do
    let tag = PTag field 0
    let v = PVVariant value
    PMessageEntry tag v

putFixedU32 :: Word32 -> Word32 -> PMessageEntry
putFixedU32 field value = do
    let tag = PTag field 5
    let v = PVI32 value
    PMessageEntry tag v
putFixedI32 :: Word32 -> Int32 -> PMessageEntry
putFixedI32 field value = putFixedU32 field (fromIntegral value)
putFixedF32 :: Word32 -> Float -> PMessageEntry
putFixedF32 field value = putFixedU32 field (castFloatToWord32 value)

putFixedU64 :: Word32 -> Word64 -> PMessageEntry
putFixedU64 field value = do
    let tag = PTag field 1
    let v = PVI64 value
    PMessageEntry tag v
putFixedI64 :: Word32 -> Int64 -> PMessageEntry
putFixedI64 field value = putFixedU64 field (fromIntegral value)
putFixedF64 :: Word32 -> Double -> PMessageEntry
putFixedF64 field value = putFixedU64 field (castDoubleToWord64 value)

putLenPrefixed :: Word32 -> B.ByteString -> PMessageEntry
putLenPrefixed field value = do
    let tag = PTag field 2
    let v = PVLenPrefixed value
    PMessageEntry tag v

putValue :: PValue -> Put
putValue (PVVariant v) = putPVInt v
putValue (PVI32 v) = putWord32le v
putValue (PVI64 v) = putWord64le v
putValue (PVLenPrefixed v) = do
    putPVInt $ pvIntDirect' $ B.length v
    putLazyByteString v

putEntry :: PMessageEntry -> Put
putEntry (PMessageEntry tag value) = do
    putTag tag
    putValue value


