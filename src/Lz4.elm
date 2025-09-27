port module Lz4 exposing (Block(..), decompressBlock, blockDecompressed)

import Json.Decode as Decode

type Block
  = Decompressed String
  | Error String
  | CommunicationError Decode.Error

decompressBlock : String -> Cmd msg
decompressBlock = lz4DecompressBlock

blockDecompressed : (Block -> msg) -> Sub msg
blockDecompressed tagger =
  Sub.batch
    [ lz4BlockDecompressed (Decompressed>>tagger)
    , lz4Error (portMap>>tagger)
    ]

portMap : Decode.Value -> Block
portMap value =
  case Decode.decodeValue errorDecoder value of
    Ok message -> Error message
    Err error -> CommunicationError error

errorDecoder : Decode.Decoder String
errorDecoder =
  Decode.field "message" Decode.string

port lz4DecompressBlock : String -> Cmd msg
port lz4BlockDecompressed : (String -> msg) -> Sub msg
port lz4Error : (Decode.Value -> msg) -> Sub msg
