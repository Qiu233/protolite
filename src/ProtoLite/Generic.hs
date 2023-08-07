{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
module ProtoLite.Generic (
    Optional, Repeated(..), Packed(..), Variant(..), SInt32(..), SInt64(..),
    ProtoField(..), ProtoBuf, encode, decode
) where
import GHC.Generics
import ProtoLite.Types
import Data.Kind
import GHC.TypeLits
import Data.Word
import ProtoLite.Encode
import Data.Proxy
import ProtoLite.Decode
import qualified Data.ByteString.Lazy as B
import Data.Int
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.IsList (IsList)
import Data.String
import Data.Binary (Put, Get)
import Data.Binary.Put (runPut, putWord32le, putWord64le, putInt32le, putInt64le, putFloatle, putDoublele)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.Binary.Get (runGet, isEmpty, getWord32le, getWord64le, getInt32le, getInt64le, getFloatle, getDoublele)
import Data.Bits



type Optional = Maybe
newtype Repeated t = Repeated { repreated :: [t] } deriving (Show, Eq, IsList)
newtype Packed t = Packed { packed :: [t] } deriving (Show, Eq, IsList)
newtype Variant t = Variant { variant :: t } deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)
newtype SInt32 = SInt32  { sint32 :: Int32 } deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)
newtype SInt64 = SInt64  { sint64 :: Int64 } deriving (Show, Eq, Num, Ord, Real, Enum, Integral, Bits)


encode :: ProtoBuf a => a -> B.ByteString
encode = runPut . pput

decode :: ProtoBuf a => B.ByteString -> a
decode bs = do
    let entries = runGet getMessage bs
    pget entries

newtype ProtoField (t :: Type) (n :: Nat) = ProtoField { protoVal :: t }
    deriving (Eq, Num, IsString, IsList, Fractional)

class GProtoBuf f where
    gput :: f a -> Put
    gget :: [PMessageEntry] -> f a
    gdef :: f a

class ProtoBuf a where
    pput :: a -> Put
    default pput :: (Generic a, GProtoBuf (Rep a)) => a -> Put
    pput = gput . from

    pget :: [PMessageEntry] -> a
    default pget :: (Generic a, GProtoBuf (Rep a)) => [PMessageEntry] -> a
    pget = to . gget

    pdef :: a
    default pdef :: (Generic a, GProtoBuf (Rep a)) => a
    pdef = to gdef

instance ProtoBuf a => GProtoBuf (K1 i a) where
    gput (K1 a) = pput a
    gget = K1 . pget
    gdef = K1 pdef

instance GProtoBuf p => GProtoBuf (M1 i t p) where
    gput (M1 a) = gput a
    gget = M1 . gget
    gdef = M1 gdef

instance (GProtoBuf f, GProtoBuf g) => GProtoBuf (f :*: g) where
    gput (f :*: g) = gput f >> gput g
    gget = (:*:) <$> gget <*> gget
    gdef = gdef :*: gdef


instance Show t => Show (ProtoField t n) where
    show (ProtoField t) = show t

class ProtoData t where
    putpd :: Word32 -> t -> Put
    getpd :: Word32 -> [PMessageEntry] -> [t]
    defpd :: t

instance (KnownNat n, ProtoData t) => ProtoBuf (ProtoField t n) where
    pput (ProtoField t) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putpd n t
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        case getpd n vs of
            [v] -> ProtoField v
            _ -> error $ "no required field " ++ show n
    pdef = ProtoField defpd


instance {-# OVERLAPPING #-} (ProtoData t, KnownNat n) => ProtoBuf (ProtoField (Maybe t) n) where
    pput (ProtoField Nothing) = pure ()
    pput (ProtoField (Just t)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putpd n t
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        case getpd n vs of
            [v] -> ProtoField $ Just v
            _ -> ProtoField Nothing
    pdef = ProtoField Nothing

instance (ProtoBuf a) => (ProtoData a) where
    defpd = pdef
    putpd n v = do
        let bs = runPut $ pput v
        putEntry $ putLenPrefixed n bs
    getpd n vs = do
        let ts = plookupAll n vs
        [pget rs
            | PVLenPrefixed s <- ts, let rs = runGet getMessage s]

instance {-# OVERLAPPING #-} ProtoData Word32 where
    defpd = 0
    putpd n v = putEntry $ putFixedU32 n v
    getpd n vs = do
        let ts = plookupAll n vs
        [s | PVI32 s <- ts]

instance {-# OVERLAPPING #-} ProtoData Int32 where
    defpd = 0
    putpd n v = putEntry $ putFixedI32 n v
    getpd n vs = do
        let ts = plookupAll n vs
        [fromIntegral s | PVI32 s <- ts]

instance {-# OVERLAPPING #-} ProtoData Float where
    defpd = 0
    putpd n v = putEntry $ putFixedF32 n v
    getpd n vs = do
        let ts = plookupAll n vs
        [castWord32ToFloat s | PVI32 s <- ts]

instance {-# OVERLAPPING #-} ProtoData Word64 where
    defpd = 0
    putpd n v = putEntry $ putFixedU64 n v
    getpd n vs = do
        let ts = plookupAll n vs
        [s | PVI64 s <- ts]

instance {-# OVERLAPPING #-} ProtoData Int64 where
    defpd = 0
    putpd n v = putEntry $ putFixedI64 n v
    getpd n vs = do
        let ts = plookupAll n vs
        [fromIntegral s | PVI64 s <- ts]

instance {-# OVERLAPPING #-} ProtoData Double where
    defpd = 0
    putpd n v = putEntry $ putFixedF64 n v
    getpd n vs = do
        let ts = plookupAll n vs
        [castWord64ToDouble s | PVI64 s <- ts]

instance {-# OVERLAPPING #-} ProtoData String where
    defpd = ""
    putpd n v = putEntry $ putLenPrefixed n (fromString v)
    getpd n vs = do
        let ts = plookupAll n vs
        [toString s | PVLenPrefixed s <- ts]

instance {-# OVERLAPPING #-} ProtoData B.ByteString where
    defpd = B.empty
    putpd n v = putEntry $ putLenPrefixed n v
    getpd n vs = do
        let ts = plookupAll n vs
        [s | PVLenPrefixed s <- ts]

instance {-# OVERLAPPING #-} (ProtoData t, KnownNat n) => ProtoBuf (ProtoField (Repeated t) n) where
    pput (ProtoField (Repeated ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        mapM_ (putpd n) ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Repeated $ getpd n vs
    pdef = ProtoField $ Repeated []

class VariantValue t where
    vvdef :: t
    vvto :: t -> PVInt
    vvfrom :: PVInt -> t

instance VariantValue Word32 where{ vvdef = 0; vvto = pvIntDirect'; vvfrom = pvIntDirect }
instance VariantValue Word64 where{ vvdef = 0; vvto = pvIntDirect'; vvfrom = pvIntDirect }
instance VariantValue Int32 where{ vvdef = 0; vvto = pvIntDirect'; vvfrom = pvIntDirect }
instance VariantValue Int64 where{ vvdef = 0; vvto = pvIntDirect'; vvfrom = pvIntDirect }
instance VariantValue SInt32 where{ vvdef = 0; vvto = pvIntDirect'; vvfrom = pvIntDirect }
instance VariantValue SInt64 where{ vvdef = 0; vvto = pvSInt'; vvfrom = pvSInt }
instance VariantValue Bool where
    vvdef = False
    vvto = \case
        False -> pvIntDirect' (0 :: Int32)
        True -> pvIntDirect' (1 :: Int32)
    vvfrom = \case
        PVInt 0 -> False
        PVInt 1 -> True
        _ -> error "not a bool"

instance {-# OVERLAPPING #-} VariantValue t => ProtoData (Variant t) where
    defpd = Variant vvdef
    putpd n (Variant v) = putEntry $ putPVIntIndex n (vvto v)
    getpd n vs = do
        let ts = plookupAll n vs
        [Variant $ vvfrom s | PVVariant s <- ts]

getMany :: Get a -> Get [a]
getMany getter = do
    isEmpty >>= \case
        True -> return []
        False -> do
            x <- getter
            xs <- getMany getter
            return (x:xs)

instance {-# OVERLAPPING #-} (KnownNat n) => ProtoBuf (ProtoField (Packed Word32) n) where
    pput (ProtoField (Packed ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ putWord32le ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Packed $ concat [runGet (getMany getWord32le) s | PVLenPrefixed s <- plookupAll n vs]
    pdef = ProtoField $ Packed []

instance {-# OVERLAPPING #-} (KnownNat n) => ProtoBuf (ProtoField (Packed Word64) n) where
    pput (ProtoField (Packed ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ putWord64le ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Packed $ concat [runGet (getMany getWord64le) s | PVLenPrefixed s <- plookupAll n vs]
    pdef = ProtoField $ Packed []

instance {-# OVERLAPPING #-} (KnownNat n) => ProtoBuf (ProtoField (Packed Int32) n) where
    pput (ProtoField (Packed ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ putInt32le ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Packed $ concat [runGet (getMany getInt32le) s | PVLenPrefixed s <- plookupAll n vs]
    pdef = ProtoField $ Packed []

instance {-# OVERLAPPING #-} (KnownNat n) => ProtoBuf (ProtoField (Packed Int64) n) where
    pput (ProtoField (Packed ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ putInt64le ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Packed $ concat [runGet (getMany getInt64le) s | PVLenPrefixed s <- plookupAll n vs]
    pdef = ProtoField $ Packed []

instance {-# OVERLAPPING #-} (KnownNat n) => ProtoBuf (ProtoField (Packed Float) n) where
    pput (ProtoField (Packed ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ putFloatle ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Packed $ concat [runGet (getMany getFloatle) s | PVLenPrefixed s <- plookupAll n vs]
    pdef = ProtoField $ Packed []

instance {-# OVERLAPPING #-} (KnownNat n) => ProtoBuf (ProtoField (Packed Double) n) where
    pput (ProtoField (Packed ts)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ putDoublele ts
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        ProtoField $ Packed $ concat [runGet (getMany getDoublele) s | PVLenPrefixed s <- plookupAll n vs]
    pdef = ProtoField $ Packed []

instance {-# OVERLAPPING #-} (KnownNat n, VariantValue t) => ProtoBuf (ProtoField (Packed (Variant t)) n) where
    pput (ProtoField (Packed vs)) = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        putEntry $ putLenPrefixed n $ runPut $ mapM_ (\(Variant v) -> putPVInt . vvto $ v) vs
    pget vs = do
        let n = fromIntegral $ natVal (Proxy :: Proxy n)
        let rs = Variant . vvfrom <$> concat [runGet (getMany getPVInt) s | PVLenPrefixed s <- plookupAll n vs]
        ProtoField $ Packed rs
    pdef = ProtoField $ Packed []