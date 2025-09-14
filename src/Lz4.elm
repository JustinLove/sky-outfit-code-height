port module Lz4 exposing (decompressBlock, blockDecompressed, error)

import Json.Decode as Decode

decompressBlock : String -> Cmd msg
decompressBlock = lz4DecompressBlock

blockDecompressed : (String -> msg) -> Sub msg
blockDecompressed = lz4BlockDecompressed

error : (Result Decode.Error String -> msg) -> Sub msg
error msg =
  lz4Error (portMap msg)

portMap : (Result Decode.Error String -> msg) -> Decode.Value -> msg
portMap msg =
  (Decode.decodeValue errorDecoder) >> msg

errorDecoder : Decode.Decoder String
errorDecoder =
  Decode.field "message" Decode.string

port lz4DecompressBlock : String -> Cmd msg
port lz4BlockDecompressed : (String -> msg) -> Sub msg
port lz4Error : (Decode.Value -> msg) -> Sub msg
