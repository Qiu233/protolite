{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ProtoLite.Aliases where
import ProtoLite.Generic
import Data.Int
import Data.Word


type VInt32 = Variant Int32
type VInt64 = Variant Int64
type VUInt32 = Variant Word32
type VUInt64 = Variant Word64
type VSInt32 = Variant SInt32
type VSInt64 = Variant SInt64
type VBool = Variant Bool

type Fixed32 = Word32
type Fixed64 = Word64
type SFixed32 = Int32
type SFixed64 = Int64

type ProtoFieldOptional t = ProtoField (Optional t)
type ProtoFieldRepeated t = ProtoField (Repeated t)
type ProtoFieldPacked t = ProtoField (Packed t)