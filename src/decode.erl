%% @doc Backend source for the decoding of erl_bencode

-module(decode).

-export([decode/1]).

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
read_next([H | Tail]) -> parse_result(read_string(Tail, list_to_integer([H]))).


parse_result({error, Reason}) ->  exit({error, Reason});
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
            {NewStr, List};
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


-spec read_string(Str, Number) -> RetStr | {error, Reason} when
    Str     :: list(),
    Number  :: integer(),
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


-spec get_string_tuple(Str, Number) -> {Tail, Data} when
    Str     :: list(),
    Number  :: integer(),
    Tail    :: list(),
    Data    :: list().
get_string_tuple(String, Num) ->
    case lists:sublist(String, Num) of
        [] ->
            {error, eof};
        Data ->
            {lists:nthtail(Num, String), Data}
        end.

-spec decode(Value) -> {ok, Data} | {error, Reason} | {warning, Reason, Data} when
    Value   :: atom() | list(),
    Data    :: map() | list() | integer(),
    Reason  :: atom().
decode(List) when is_list(List) -> read_next(List);
decode(Filename) when is_atom(Filename)->
    {ok, Bin} = file:read_file(Filename),
    read_next(binary_to_list(Bin)).
