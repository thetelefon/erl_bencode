-module(encode).

-export([start/1]).

-import(string, [concat/2]).

-spec start_encode(Encode, List) -> RetList | {error, Reason} when
    Encode  :: term(),
    List    :: list(),
    RetList :: list(),
    Reason  :: atom().
start_encode(Encode, List) when is_map(Encode)       -> encode_map(Encode, List);
start_encode(Encode, List) when is_list(Encode)      -> determ_list_or_string(Encode, List);
start_encode(Encode, List) when is_number(Encode)    -> encode_int(Encode, List);
start_encode(_,_) -> {error, badform}.

-spec determ_list_or_string(Encode, List) -> RetList when
    Encode  :: term(),
    List    :: list(),
    RetList :: list().
determ_list_or_string(Encode, List) ->
    Bool = io_lib:char_list(Encode),
    if Bool ->
        encode_string(Encode, List);
    true ->
        encode_list(Encode, List)
    end.

-spec encode_list(Encode, List) -> RetList when
    Encode  :: term(),
    List    :: list(),
    RetList :: list().
encode_list(Encode, List) ->
    TempHead = iter_list(Encode, concat(List, "l")),
    NewHead = concat(TempHead, "e"),
    NewHead.

-spec iter_list(Encode, List) -> RetList when
    Encode  :: term(),
    List    :: list(),
    RetList :: list().
iter_list([], List) -> List;
iter_list([H | T], List) ->
    NewHead = start_encode(H, List),
    iter_list(T, NewHead).

-spec encode_map(Encode, List) -> RetList when
    Encode  :: term(),
    List    :: list(),
    RetList :: list().
encode_map(Encode, List) ->
    ListOfKeys = maps:keys(Encode),
    TempHead = iter_map(Encode, concat(List, "d"), ListOfKeys),
    NewHead = concat(TempHead, "e"),
    NewHead.

-spec iter_map(Encode, List, Keys) -> RetList when
    Encode  :: term(),
    List    :: list(),
    Keys    :: list(),
    RetList :: list().
iter_map(_, List, []) -> List;
iter_map(Encode, List, [Key | Tail]) ->
    TempHead = start_encode(Key, List),
    NewHead = start_encode(maps:get(Key, Encode), TempHead),
    iter_map(Encode, NewHead, Tail).

-spec encode_int(Encode, List) -> RetList when
    Encode  :: term(),
    List    :: list(),
    RetList :: list.
encode_int(Encode, List) ->
    IntList = integer_to_list(Encode),
    concat(List, concat(concat("i", IntList), "e")).

-spec encode_string(Encode, List) -> RetList when
    Encode  :: term(),
    List    :: list(),
    RetList :: list().
encode_string(Encode, List) ->
    Len = string:length(Encode),
    concat(List, concat(concat(integer_to_list(Len), ":"), Encode)).

-spec start(Encode) -> Value | {error, Reason} when
    Encode  :: term(),
    Value   :: list(),
    Reason  :: atom().
start(Encode) ->
    Value = start_encode(Encode, []),
    case Value of
        {error, _Reason} ->
            Value;
        Value ->
            Value
        end.
