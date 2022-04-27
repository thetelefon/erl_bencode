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
%% @end
-spec encode(Value) -> {ok, Encoded} | {error, Reason} | {warning, Reason, Encoded} when
    Value       :: map() | list() | integer(),
    Encoded     :: list(),
    Reason      :: atom().
encode(Value) -> encode:'_encode'(Value).


%% @doc Decodes `Value' to a valid erlang term().
%% 
%% If `Value' is an atom() it's considerd to be a file path.
%% 
%% If `Value' is a list() it's considerd to be a bencode string
%% @end
-spec decode(Value) -> {ok, Decoded} | {error, Reason} | {warning, Reason, Decoded} when
    Value           :: list() | atom(),
    Decoded         :: map() | list() | integer(),
    Reason          :: atom().
decode(Value) -> decode:'_decode'(Value).


%% ===============================
%%       Integration Tests
%% ===============================

%% @private
encode_test() ->
    Map = #{"Warden" => 23, 534 => "test", "A list" => [54, #{}, [], "clyffe"]},
    {ok, EncodedMap} = encode:'_encode'(Map),
    ?assert(EncodedMap =:= "di534e4:test6:A listli54edele6:clyffee6:Wardeni23ee"),
    {error, Reason} = encode:'_encode'(map),
    ?assert(Reason =:= badargs).

%% @private
decode_test() ->
    Bencode = "di534e4:test6:A listli54edele6:clyffee6:Wardeni23ee",
    {ok, Map} = decode:'_decode'(Bencode),
    ?assert(Map =:= #{534 => "test", "A list" => ["clyffe", [], #{}, 54], "Warden" => 23}),
    {error, Reason} = decode:'_decode'(test_atom),
    ?assert(Reason =:= enoent).

