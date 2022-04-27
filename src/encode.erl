%% @private

-module(encode).

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
