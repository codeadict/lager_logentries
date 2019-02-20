-module(lager_logentries_utils).

-export([parse_config/1,
         validate_loglevel/1,
         safe_json/1
        ]).

-type config() :: #{host := lager_logentries:host(),
                    port := lager_logentries:port_number(),
                    address_family := lager_logentries:address_family(),
                    level := lager_logentries:mask(),
                    token := iolist() | binary(),
                    context := [{atom(), jsx:json_term()}]
                   }.

%% API
-spec parse_config([{atom(), any()}]) -> {ok, config()} | {error, term()}.
parse_config(Config) when is_list(Config) ->
    Level = proplists:get_value(level, Config, info),
    Host = proplists:get_value(host, Config, <<"data.logentries.com">>),
    Port = proplists:get_value(port, Config, 80),
    AddressFamily = proplists:get_value(address_family, Config),
    Token = proplists:get_value(token, Config),
    Context = proplists:get_value(context, Config, []),
    DefaultConfig = [{level, Level},
                     {host, Host},
                     {port, Port},
                     {address_family, AddressFamily},
                     {token, Token},
                     {context, Context}],
    validate_settings(DefaultConfig, #{}).

%% Private

-spec validate_settings([{atom(), term()}], map()) ->
                               {ok, config()} | {error, term()}.
validate_settings([{Key, Value} | Rest], Acc) ->
    case validate_setting(Key, Value) of
        ok ->
            validate_settings(Rest, Acc#{Key => Value});
        {ok, NewValue} ->
            validate_settings(Rest, Acc#{Key => NewValue});
        {error, Error} ->
            {error, Error}
    end;
validate_settings([], Acc) ->
    {ok, Acc}.

-spec validate_loglevel(any()) -> {ok, lager_logentries:mask()} | error.
validate_loglevel(Level) ->
    try lager_util:config_to_mask(Level) of
        Mask ->
            {ok, Mask}
    catch _:_ ->
            error
    end.

-spec is_uuid4(iolist() | binary()) -> boolean().
is_uuid4(Uuid) when is_list(Uuid) ->
    is_uuid4(iolist_to_binary(Uuid));
is_uuid4(<<_:36/binary>> = Uuid) when is_binary(Uuid)->
    true;
is_uuid4(_) ->
    false.

-spec validate_setting(atom(), term()) -> ok | {ok, term()} | {error, term()}.
validate_setting(level, Level) when is_atom(Level) ->
    case validate_loglevel(Level) of
        {ok, Mask} -> {ok, Mask};
        error -> {error, {invalid_loglevel, Level}}
    end;
validate_setting(host, undefined) ->
    {error, undefined_host};
validate_setting(port, undefined) ->
    {error, undefined_port};
validate_setting(port, P) when not is_integer(P) ->
    {error, {invalid_port, P}};
validate_setting(port, Port) when Port < 1 orelse Port > 65536 ->
    {error, {invalid_port, Port}};
validate_setting(address_family, Family) when Family =/= undefined,
                                              Family =/= inet,
                                              Family =/= inet6 ->
    {error, {invalid_address_family, Family}};
validate_setting(token, undefined) ->
    {error, undefined_token};
validate_setting(token, Token) ->
    case is_uuid4(Token) of
        true ->
            {ok, Token};
        false ->
            {error, {invalid_token, Token}}
    end;
validate_setting(context, Context) when not is_list(Context) ->
    {error, {invalid_context, Context}};
validate_setting(context, Context) ->
    {ok, safe_json(Context)};
validate_setting(_, _) ->
    ok.

-spec safe_json([{term(), term()}]) -> [lager_logentries:json_object()].
safe_json(Fields) ->
    lists:map(fun to_json/1, Fields).

-spec to_json({term(), term()}) -> lager_logentries:json_object().
to_json({Key, Value}) when is_atom(Key);
                           is_binary(Key)->
    {Key, json_value(Value)};
to_json({Key, Value}) when is_list(Key) ->
    to_json({list_to_binary(Key), Value}).

-spec json_value(term()) -> jsx:json_term().
json_value(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
json_value(List) when is_list(List) ->
    case io_lib:char_list(List) of
        true ->
            list_to_binary(List);
        false ->
            lists:map(fun json_value/1, List)
    end;
json_value(Val) ->
    Val.
