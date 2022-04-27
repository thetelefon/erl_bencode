%% @private

-module(encode).
-include_lib("eunit/include/eunit.hrl").

-export(['_encode'/1]).
-import(string, [concat/2]).

-spec start_encode(Encode, List) -> RetList | {error, Reason} when
    Encode  :: term(),
    List    :: list(),
    RetList :: list(),
    Reason  :: atom().
start_encode(Encode, List) when is_map(Encode)       -> encode_map(Encode, List);
start_encode(Encode, List) when is_list(Encode)      -> determ_list_or_string(Encode, List);
start_encode(Encode, List) when is_number(Encode)    -> encode_int(Encode, List);
start_encode(_,_) -> {error, badargs}.

-spec determ_list_or_string(Encode, List) -> RetList when
    Encode  :: list(),
    List    :: list(),
    RetList :: list().
determ_list_or_string([], List) -> encode_list([], List);
determ_list_or_string(Encode, List) ->
    Bool = io_lib:char_list(Encode),
    if Bool ->
        encode_string(Encode, List);
    true ->
        encode_list(Encode, List)
    end.

-spec encode_list(Encode, List) -> {ok, RetList} when
    Encode  :: list(),
    List    :: list(),
    RetList :: list().
encode_list(Encode, _) when not(is_list(Encode)) -> {error, not_list};
encode_list(Encode, List) ->
    {ok, TempHead} = iter_list(Encode, concat(List, "l")),
    {ok, concat(TempHead, "e")}.

-spec iter_list(Encode, List) -> {ok, RetList} when
    Encode  :: list(),
    List    :: list(),
    RetList :: list().
iter_list([], List) -> {ok, List};
iter_list([H | T], List) ->
    {ok, NewHead} = start_encode(H, List),
    iter_list(T, NewHead).

-spec encode_map(Encode, List) -> {ok, RetList} when
    Encode  :: map(),
    List    :: list(),
    RetList :: list().
encode_map(Encode, _) when not(is_map(Encode)) -> {error, not_map};
encode_map(Encode, List) ->
    ListOfKeys = maps:keys(Encode),
    {ok, TempHead} = iter_map(Encode, concat(List, "d"), ListOfKeys),
    {ok, concat(TempHead, "e")}.

-spec iter_map(Encode, List, Keys) -> {ok, RetList} when
    Encode  :: map(),
    List    :: list(),
    Keys    :: list(),
    RetList :: list().
iter_map(_, List, []) -> {ok, List};
iter_map(Encode, List, [Key | Tail]) ->
    {ok, TempHead} = start_encode(Key, List),
    {ok, NewHead} = start_encode(maps:get(Key, Encode), TempHead),
    iter_map(Encode, NewHead, Tail).

-spec encode_int(Encode, List) -> {ok, RetList} when
    Encode  :: integer(),
    List    :: list(),
    RetList :: list().
encode_int(Encode, _) when not(is_integer(Encode)) -> {error, not_number};
encode_int(Encode, List) ->
    IntList = integer_to_list(Encode),
    {ok, concat(List, concat(concat("i", IntList), "e"))}.

-spec encode_string(Encode, List) -> {ok, RetList} when
    Encode  :: list(),
    List    :: list(),
    RetList :: list().
encode_string([], _) -> {error, zero_length};
encode_string(Encode, _) when not(is_list(Encode)) -> {error, not_list}; 
encode_string(Encode, List) ->
    Len = string:length(Encode),
    {ok, concat(List, concat(concat(integer_to_list(Len), ":"), Encode))}.

-spec '_encode'(Encode) -> {ok, Value} | {error, Reason} when
    Encode  :: term(),
    Value   :: list(),
    Reason  :: atom().
'_encode'(Encode) ->
    Value = start_encode(Encode, []),
    case Value of
        {error, _Reason} ->
            Value;
        Value ->
            Value
        end.

%% ----------------------------------
%%           Unit tests
%% ----------------------------------

start_encode_test() ->
    ?assert(start_encode(["test", 23, "warden", #{"clyffe" => 43}], []) =:= {ok, "l4:testi23e6:wardend6:clyffei43eee"}),
    ?assert(start_encode(atom, [])                                      =:= {error, badargs}),
    ?assert(start_encode("warden",[])                                   =:= {ok, "6:warden"}),
    ?assert(start_encode(23, [])                                        =:= {ok, "i23e"}),
    ?assert(start_encode(#{}, [])                                       =:= {ok, "de"}).

determ_list_or_string_test() ->
    ?assert(determ_list_or_string("Warden", [])   =:= {ok, "6:Warden"}),
    ?assert(determ_list_or_string(["Warden"], []) =:= {ok, "l6:Wardene"}),
    ?assert(determ_list_or_string(23, [])         =:= {error, not_list}),
    ?assert(determ_list_or_string("", [])         =:= {ok, "le"}).


encode_list_test() ->
    ?assert(encode_list([23,43,54], []) =:= {ok, "li23ei43ei54ee"}),
    ?assert(encode_list([],[])          =:= {ok, "le"}),
    ?assert(encode_list(#{23=> 32}, []) =:= {error, not_list}).


encode_map_test() ->
    ?assert(encode_map(#{}, [])           =:= {ok, "de"}),
    ?assert(encode_map(#{23 => "23"}, []) =:= {ok, "di23e2:23e"}),
    ?assert(encode_map("Warden", [])      =:= {error, not_map}).


encode_int_test() ->
    ?assert(encode_int(23, [])               =:= {ok, "i23e"}),
    ?assert(encode_int("23", [])             =:= {error, not_number}),
    ?assert(encode_int(3125452313, "l3:hej") =:= {ok, "l3:heji3125452313e"}).


encode_string_test() ->
    ?assert(encode_string("hej", []) =:= {ok, "3:hej"}),
    ?assert(encode_string("", [])    =:= {error, zero_length}),
    ?assert(encode_string("23", [])  =:= {ok, "2:23"}),
    ?assert(encode_string(#{}, [])   =:= {error, not_list}).
