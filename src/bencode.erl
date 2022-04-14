-module(bencode).
-export([encode_term/1, decode_term/1, encode_file/1, decode_file/1]).

encode_term(Value) -> encode:start(Value).
decode_term(Value) -> decode:start_term(Value).

encode_file(FilePath) -> encode:start_file(FilePath).
decode_file(FilePath) -> decode:start(FilePath).