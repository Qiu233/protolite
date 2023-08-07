# protolite
`protolite` is a Haskell protobuf en/decoder library that exploits `GHC.Generics` and `DataKinds`.
## why protolite
- &#10004; Nested message supported.
- &#10004; All kinds of protobuf data supported.
- &#10004; Very clean. Neither code generation nor templates. Protobuf definition inferred easily.
- &#10004; Lightweight. Tiny code base, dependent only to `binary`, `bytestring` and `utf8-string`.

# quick start
**More examples in** [test/Spec.hs](./test/Spec.hs).
```Haskell

data ProtoSkipped = ProtoSkipped {
    skipped1 :: ProtoField Word32 1,
    skipped2 :: ProtoField (Optional Word32) 3,
    skipped3 :: ProtoField (Optional String) 10,
    skipped4 :: ProtoField OtherMessage 20
} deriving (Show, Eq, Generic)
instance ProtoBuf ProtoSkipped
```
The above snippet defines a protobuf message consisting of four fields. It's equivalent to the following protobuf definition.

```protobuf
message ProtoSkipped {
    fixed32 skipped1 = 1;
    optional fixed32 skipped2 = 3;
    optional string skipped3 = 10;
    OtherMessage skipped4 = 20;
}
```

Then it can be consumed as
```Haskell
let v = ProtoSkipped {
        skipped1 = 0x12345678,
        skipped2 = ProtoField $ Just 0x12345678,
        skipped3 = ProtoField $ Just "hello",
        skipped4 = ...
}
let bs = encode v
let v' = decode bs :: ProtoSkipped
print v
printHex bs
print v'
```

# types from/to protobuf
## prerequisites
In protobuf there're exactly three kinds of data: `variant`, `fixed`, and `len-prefixed`.  
- `variant` includes integral data that has variant length depending on its value. This type is designed for space efficiency.
- `fixed` includes exactly what's from C-like language, `int32`, `uint32`, `float`, `int64`, `uint64`, `double`, which all have a compile-time fixed length.
- `len-prefixed` includes strings, bytearrays, packed arrays, and even the nested messages.

More to be found here [protobuf-reference](https://protobuf.dev/programming-guides/encoding/).

## types
In the following is the type bimap between protobuf and protolite.  
Types in the colomn `protolite` can replace symbol `*` in `ProtoField (*) n` to form a field signature.  
The `t` means it is replaced by some other defined types. But exceptionally, only `fixed` and `variant` can be applied to `packed`.  
Last thing to note, `Optional` is nothing more than an alias of `Maybe`.
| kind | protobuf | protolite |
|:----:|:----|:----|
|general| repeated `t` | Repeated `t` |
|       | optional `t` | Optional `t` \| Maybe `t` |
| fixed |sfixed32  |Int32 |
|       |fixed32 |Word32|
|       |sfixed64  |Int64 |
|       |fixed64 |Word64|
|       |float  |Float  |
|       |double  |Double  |
| variant | int32 | Variant Int32 |
|       | int64 | Variant Int64 |
|       | uint32 | Variant Word32 |
|       | uint64 | Variant Word64 |
|       | sint32 | Variant SInt32 |
|       | sint64 | Variant SInt64 |
|       | enum   | Variant Int32  |
|       | bool   | Variant Bool   |
| len-prefixed | string | String |
|       | bytes | LazyByteString |
|       | packed `t` | Packed `t` |
|       | *message* | *message* |
