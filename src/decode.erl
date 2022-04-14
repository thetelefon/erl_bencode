-module(decode).

-export([start/1]).

read_file(IO) ->
    Tuple = file:read(IO, 1),
    case Tuple of
        eof ->
            {error, eof_read_file};
        {error, _Reason} ->
            Tuple;
        {ok, <<"i">>} ->
            check_result(read_number(IO));
        {ok, <<"d">>} ->
            check_result(get_dict(IO));
        {ok, <<"l">>} ->
            check_result(get_list(IO));
        {ok, <<"e">>} ->
            eof;
        {ok, Nr} ->
            check_result(read_string(IO, Nr))
        end.

%% ============================ %%

check_result({error, Reason}) ->  exit({error, Reason});
check_result(Value) -> Value.

%% ============================ %%

-spec get_list(IO) -> List | {error, Reason} when
    IO      :: file:io_device(),
    List    :: list(),
    Reason  :: atom().
get_list(IO) -> get_list(IO, []).

-spec get_list(IO, List) -> RetList | {error, Reason} when
    IO      :: file:io_device(),
    List    :: list(),
    RetList :: list(),
    Reason  :: atom().
get_list(IO, List) ->
    Element = read_file(IO),
    case Element of
        eof ->
            List;
        {error, _} ->
            io:format("get list\n"),
            Element;
        Element ->
            get_list(IO, [Element | List])
        end.

%% ============================ %%

-spec get_dict(IO) -> Dict | {error, Reason} | {warning, Reason, Dict} when
    IO      :: file:io_device(),
    Dict    :: map(),
    Reason  :: atom().
get_dict(IO) -> get_dict(IO, #{}).

-spec get_dict(IO, Dict) -> RetDict | {error, Reason} | {warning, Reason, RetDict} when
    IO      :: file:io_device(),
    Dict    :: map(),
    RetDict :: map(),
    Reason  :: atom().
get_dict(IO, Dict) ->
    Tuple = file:read(IO, 1),
    case Tuple of
        eof ->
            Dict;
        {error, _Reason} ->
            Tuple;
        {ok, <<"i">>} ->
            Key = read_number(IO),
            Value = read_file(IO),
            case Value of
                {warning, Reason, WarVal} ->
                    {warning, Reason, get_dict(IO, maps:put(Key, WarVal, Dict))};
                Value ->
                    get_dict(IO, maps:put(Key, Value, Dict))
                end;
        {ok, <<"d">>} ->
            {error, dict_as_key};
        {ok, <<"l">>} ->
            {error, list_as_key};
        {ok, <<"e">>} ->
            Dict;
        {ok, Nr} ->
            Bool = test_if_integer(Nr),
            if Bool ->
                Key = read_string(IO, Nr),
                Value = read_file(IO),
                case Value of
                    {warning, _Reason, WarVal} ->
                        io:format("==>\t\tWARNING\n"),
                        get_dict(IO, maps:put(Key, WarVal, Dict));
                    Value ->
                        get_dict(IO, maps:put(Key, Value, Dict))
                    end;
            true ->
                {warning, corrupt, Dict}
            end
        end.

%% ============================ %%
-spec read_number(IO) -> Number | {error, Reason} when
    IO      :: file:io_device(),
    Number  :: integer(),
    Reason  :: atom().
read_number(IO) -> read_number(IO, 0).

-spec read_number(IO, Acc) -> Number | {error, Reason} when
    IO      :: file:io_device(),
    Acc     :: integer(),
    Number  :: integer(),
    Reason  :: atom().
read_number(IO, Acc) ->
    Tuple = file:read(IO, 1),
    case Tuple of
        eof ->
            {error, eof};
        {error, _Reason} ->
            Tuple;
        {ok, Data} ->
            case Data of
                <<":">> ->
                    Acc;
                <<"e">> ->
                    Acc;
                Nr ->
                    Bool = test_if_integer(Nr),
                    if Bool ->
                        read_number(IO, Acc*10+ binary_to_integer(Nr));
                    true ->
                        {error, badargs}
                    end
                end
        end.

%% ============================ %%
-spec test_if_integer(Bin) -> Bool when
    Bin     :: binary(),
    Bool    :: boolean().

test_if_integer(Bin) ->
    try
        _ = binary_to_integer(Bin),
        true
    catch error:badarg ->
        false
    end.

%% ============================ %%

-spec read_string(IO, Bin) -> Str | eof | {error, Reason} when
    IO      :: file:io_device(),
    Bin     :: binary(),
    Str     :: string(),
    Reason  :: atom().

read_string(IO, Bin) ->
    Bool = test_if_integer(Bin),
    if Bool ->
        NrOfBytes = read_number(IO, binary_to_integer(Bin)),
        Tuple = file:read(IO, NrOfBytes),

        case Tuple of
            eof ->
                {error, eof};
            {error, _Reason} ->
                Tuple;
            {ok, Data} ->
                binary_to_list(Data)
            end;
    true ->
        {error, not_a_number}
    end.

%% ============================ %%

-spec start(Filename) -> {ok, Value} | {error, Reason} | {warning, Reason, Value} when
    Filename    :: string(),
    Value       :: map() | string() | integer() | list(),
    Reason      :: atom().
start(Filename) ->
    Return = file:open(Filename, [read, binary]),
    case Return of
        {ok, Io} ->
            Value = read_file(Io),
            {ok, Value};
        {error, _Reason} ->
            Return
        end.
