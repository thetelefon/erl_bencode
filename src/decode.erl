%% @private

-module(decode).
-include_lib("eunit/include/eunit.hrl").

-export(['_decode'/1]).

-spec read_next(Str) -> {Tail, Result} | {ok, Result} | {error, Reason} | {warning, Reason, Result} when
    Str     :: list(),
    Tail    :: list(),
    Result  :: map() | list() | integer(),
    Reason  :: atom().
read_next([]) -> eof;
read_next([H | Tail]) when [H] == "i" -> parse_result(read_number(Tail));
read_next([H | Tail]) when [H] == "d" -> parse_result(get_dict(Tail));
read_next([H | Tail]) when [H] == "l" -> parse_result(get_list(Tail));
read_next([H | Tail]) when [H] == "e" -> {eof, Tail};
read_next([H | Tail]) -> parse_result(read_string(Tail, list_to_integer([H])));
read_next(_) -> {error, badargs}.


parse_result({error, Reason}) ->  {error, Reason};
parse_result({[], Value}) -> {ok, Value};
parse_result(Value) -> Value.

-spec get_list(Str) -> {Tail, List} | {error, Reason} | {warning, Reason, List} when
    Str     :: list(),
    Tail    :: list(),
    List    :: list(),
    Reason  :: atom().
get_list(Str) -> get_list(Str, []).

-spec get_list(Str, List) -> {Tail, RetList} | {error, Reason} | {warning, Reason, RetList} when
    Str     :: list(),
    List    :: list(),
    Tail    :: list(),
    RetList :: list(),
    Reason  :: atom().
get_list(Str, List) ->
    case read_next(Str) of
        eof ->
            {error, eof};
        {eof, NewStr} ->
            {NewStr, lists:reverse(List)};
        {error, Reason} ->
            Reason;
        {NewStr, Data} ->
            get_list(NewStr, [Data | List])
        end.

-spec get_dict(Str) -> {Tail, Dict} | {error, Reason} | {warning, Reason, Dict} when
    Str     :: map(),
    Tail    :: list(),
    Dict    :: map(),
    Reason  :: atom().
get_dict(Str) -> get_dict(Str, #{}).

-spec get_dict(Str, Dict) -> {Tail, RetDict} | {error, Reason} | {warning, Reason, RetDict} when
    Str     :: map(),
    Dict    :: map(),
    Tail    :: list(),
    RetDict :: map(),
    Reason  :: atom().
get_dict(eof, _Dict) -> {error, no_end};
get_dict([], _Dict) -> {error, no_end};
get_dict([H | _], _Dict) when [H] == "d" -> {error, dict_as_key};
get_dict([H | _], _Dict) when [H] == "l" -> {error, list_as_key};
get_dict([H | Tail], Dict) when [H] == "e" -> {Tail, Dict};
get_dict([H | Tail], Dict) when [H] == "i" -> get_key_and_value(read_number(Tail), Dict);
get_dict([H | Tail], Dict) ->
    Bool = test_if_integer([H]),
    if Bool ->
        get_key_and_value(read_string(Tail, list_to_integer([H])), Dict);
    true ->
        {Tail, {warning, corrupt, Dict}}
    end.


-spec get_key_and_value({Str, Key}, Dict) -> {Tail, RetDict} | {error, Reason} | {warning, Reason, RetDict} when
    Str     :: list(),
    Key     :: list() | integer(),
    Dict    :: map(),
    Tail    :: list(),
    RetDict :: map(),
    Reason  :: atom().
get_key_and_value({Str, Key}, Dict) ->
    {NewStr, Value} = read_next(Str),
    case Value of
        {warning, Reason, WarVal} ->
            {warning, Reason, get_dict(NewStr, maps:put(Key, WarVal, Dict))};
        Value ->
            get_dict(NewStr, maps:put(Key, Value, Dict))
        end.


-spec read_number(Str) -> {Tail, Number} | {error, Reason} when
    Str     :: list(),
    Tail    :: list(),
    Number  :: integer(),
    Reason  :: atom().
read_number([45,45 | _]) -> {error, double_negation};
read_number([45 | Tail]) -> 
    {NewStr, Number} = read_number(Tail, 0),
    if NewStr =:= error ->
        {NewStr, Number};
    true ->
        {NewStr, -1*Number}
    end;
read_number(Str) -> read_number(Str, 0).


-spec read_number(Str, Acc) -> {Tail, Number} | {error, Reason} when
    Str     :: list(),
    Acc     :: integer(),
    Tail    :: list(),
    Number  :: integer(),
    Reason  :: atom().
read_number([], _) -> {error, no_end};
read_number([H | Tail], Acc) when [H] == ":" -> {Tail, Acc};
read_number([H | Tail], Acc) when [H] == "e" -> {Tail, Acc};
read_number([H | Tail], Acc) ->
    Bool = test_if_integer([H]),
    if Bool ->
        read_number(Tail, (Acc*10)+ list_to_integer([H]));
    true ->
        {error, badargs}
    end.

-spec test_if_integer(Str) -> Bool when
    Str     :: list(),
    Bool    :: boolean().
test_if_integer(Str) ->
    try
        _ = list_to_integer(Str),
        true
    catch error:badarg ->
        false
    end.

-spec read_string(Str, Number) -> {Tail, RetStr} | {error, Reason} when
    Str     :: list(),
    Number  :: integer(),
    Tail    :: list(),
    RetStr  :: list(),
    Reason  :: atom().
read_string([H | Tail] = Str, Num) ->
    Bool = test_if_integer([H]),
    if Bool ->
        {NewStr, NrOfBytes} = read_number(Str, Num),
        get_string_tuple(NewStr, NrOfBytes);
    [H] == ":" ->
        get_string_tuple(Tail, Num);
    true ->
        {error, not_a_number}
    end.

-spec get_string_tuple(Str, Number) -> {Tail, Data} | {error, Reason} when
    Str     :: list(),
    Number  :: integer(),
    Tail    :: list(),
    Data    :: list(),
    Reason  :: term().
get_string_tuple(String, Num) when length(String) < Num -> {error, invalid_length};
get_string_tuple(String, Num) ->
    case lists:sublist(String, Num) of
        [] ->
            {error, eof};
        Data ->
            {lists:nthtail(Num, String), Data}
        end.

-spec '_decode'(Value) -> {ok, Data} | {error, Reason} | {warning, Reason, Data} when
    Value   :: atom() | list(),
    Data    :: map() | list() | integer(),
    Reason  :: atom().
'_decode'(List) when is_list(List) -> read_next(List);
'_decode'(Filename) when is_atom(Filename)->
    case file:read_file(Filename) of
        {ok, Bin} ->
            read_next(binary_to_list(Bin));
        {error, Reason} ->
            {error, Reason}
        end.


%% ----------------------------------
%%           Unit tests
%% ----------------------------------

read_next_test() ->
    ?assert(read_next([])       =:= eof),
    ?assert(read_next("i-23e")  =:= {ok, -23}),
    ?assert(read_next("i--23e") =:= {error, double_negation}),
    ?assert(read_next("i-23-e") =:= {error, badargs}),
    ?assert(read_next("4:2453") =:= {ok, "2453"}),
    ?assert(read_next("de")     =:= {ok, #{}}),
    ?assert(read_next(#{})      =:= {error, badargs}).

parse_result_test() ->
    ?assert(parse_result({error, reason}) =:= {error, reason}),
    ?assert(parse_result({[], reason})    =:= {ok, reason}),
    ?assert(parse_result("testing")       =:= "testing").

get_list_test() ->
    ?assert(get_list("i23elel2:23ee") =:= {[], [["23"],[], 23]}),
    ?assert(get_list("e2:23")         =:= {"2:23", []}).

get_dict_test() ->
    ?assert(get_dict("e")                          =:= {[], #{}}),
    ?assert(get_dict("lee")                        =:= {error, list_as_key}),
    ?assert(get_dict("dee")                        =:= {error, dict_as_key}),
    ?assert(get_dict("3:heje")                     =:= {error, no_end}),
    ?assert(get_dict("")                           =:= {error, no_end}),
    ?assert(get_dict("i23e4:teste")                =:= {[], #{23 => "test"}}),
    ?assert(get_dict("i23e3:hay6:warden6:clyffee") =:= {[], #{23 => "hay", "warden" => "clyffe"}}).

get_key_and_value_test() ->
    ?assert(get_key_and_value({"i23ee", "test"}, #{}) =:= {[], #{"test" => 23}}),
    ?assert(get_key_and_value({"lee", 23}, #{})       =:= {[],#{23 => []}}).

read_number_test() ->
    ?assert(read_number("2453e")  =:= {[], 2453}),
    ?assert(read_number("e")      =:= {[], 0}),
    ?assert(read_number("-2453e") =:= {[], -2453}),
    ?assert(read_number("2-453e") =:= {error, badargs}),
    ?assert(read_number("253")    =:= {error, no_end}),
    ?assert(read_number("25 3")   =:= {error, badargs}),
    ?assert(read_number("2d53")   =:= {error, badargs}),
    ?assert(read_number("i253e")  =:= {error, badargs}).

test_if_integer_test() ->
    ?assert(test_if_integer(23)       =:= false),
    ?assert(test_if_integer("23")     =:= true),
    ?assert(test_if_integer("2he3j5") =:= false),
    ?assert(test_if_integer("24 321") =:= false),
    ?assert(test_if_integer(" 23")    =:= false).

read_string_test() ->
    ?assert(read_string("3:heji23e", 0)     =:= {"i23e", "hej"}),
    ?assert(read_string("q6:wardeni23e", 0) =:= {error, not_a_number}),
    ?assert(read_string("10:wardeni23e", 0) =:= {[], "wardeni23e"}).

get_string_tuple_test() ->
    ?assert(get_string_tuple("Hello World", 0)  =:= {error, eof}),
    ?assert(get_string_tuple([], 10)            =:= {error, invalid_length}),
    ?assert(get_string_tuple("Hello World", 4)  =:= {"o World", "Hell"}),
    ?assert(get_string_tuple("Hello World", 11) =:= {"", "Hello World"}).
