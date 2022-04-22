%% @author Robert Lasu
%% @version 0.1.0
%% @copyright 2022 Robert Lasu

-module(bencode).
-export([encode/1, decode/1]).

%% @doc Encodes `Value' to Bencode.
%% @TODO: Known bug when encoding binaries of only numbers
%% which should be string is not correctly encoded
%% @end
-spec encode(Value) -> BencodeStr when
    Value       :: map() | list() | binary(),
    BencodeStr  :: list().
encode(Value) -> encode:start(Value).


%% @doc Decodes `Value' to a valid erlang term().
%% 
%% @end
-spec decode(Value) -> DecodedBencode when
    Value           :: list() | atom(),
    DecodedBencode  :: map() | list() | binary().
decode(Value) -> decode:start(Value).
