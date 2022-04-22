%% @author Robert Lasu
%% @version 0.1.0
%% @copyright 2022 Robert Lasu
%% @doc A simple bencode decoder and encoder.

%% ----------------------------------
%%      This is the API to use
%% ----------------------------------

-module(bencode).
-export([encode/1, decode/1]).

%% @doc Encodes `Value' to Bencode.
%% @TODO: Known bug when encoding binaries of only numbers
%% which should be string is not correctly encoded
%% @end
-spec encode(Value) -> {ok, BencodeStr} when
    Value       :: map() | list() | binary(),
    BencodeStr  :: list().
encode(Value) -> encode:encode(Value).


%% @doc Decodes `Value' to a valid erlang term().
%% 
%% @end
-spec decode(Value) -> {ok, DecodedBencode} when
    Value           :: list() | atom(),
    DecodedBencode  :: map() | list() | binary().
decode(Value) -> decode:decode(Value).
