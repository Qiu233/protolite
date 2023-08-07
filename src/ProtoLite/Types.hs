{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module ProtoLite.Types where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word
import Data.Binary

newtype PVInt = PVInt Word64

data PTag = PTag {
    _fieldNumber :: Word32,
    _wireType :: Word8
} deriving (Show)

data PValue =
    PVVariant PVInt
    | PVI32 Word32
    | PVI64 Word64
    | PVLenPrefixed B.ByteString

data PMessageEntry = PMessageEntry {
    _tag :: PTag,
    _value :: PValue
}

pvIntDirect :: Integral a => PVInt -> a
pvIntDirect (PVInt pvi) = fromIntegral pvi
pvIntDirect' :: Integral a => a -> PVInt
pvIntDirect' a = PVInt $ fromIntegral a

pvSInt :: (Bits a, Integral a) => PVInt -> a
pvSInt (PVInt pvi) =
    if even pvi then
        fromIntegral pvi `shiftR` 1
    else
        fromIntegral ((pvi + 1) `div` 2) * (-1)
pvSInt' :: (Bits a, Integral a) => a -> PVInt
pvSInt' a =
    if a >= 0 then
        PVInt $ fromIntegral (a `shiftL` 1)
    else
        PVInt $ fromIntegral (abs a * 2 - 1)

getPVInt :: Get PVInt
getPVInt = do
    bs <- getbs_
    let bs' = fromIntegral <$> bs :: [Word64]
    pure $ PVInt $ foldl (\acc b -> (acc `shiftL` 7) .|. b) 0 (reverse bs')
    where
        getbs_ = do
            v <- getWord8
            let v' = v .&. 0x7f
            if (v .&. 0x80) == 0 then
                pure [v']
            else do
                vs <- getbs_
                pure (v':vs)

putPVInt :: PVInt -> Put
putPVInt (PVInt vt) = putbs_ . go $ vt
    where
        go :: Word64 -> [Word8]
        go v =
            let v' = v .&. 0x7f
                v'' = v `shiftR` 7
            in if v'' == 0 then
                [fromIntegral v']
            else
                fromIntegral v' : go v''
        putbs_ :: [Word8] -> Put
        putbs_ [] = pure ()
        putbs_ (b:bs) = do
            if null bs then
                putWord8 b
            else do
                let b' = b .|. 0x80
                putWord8 b'
                putbs_ bs