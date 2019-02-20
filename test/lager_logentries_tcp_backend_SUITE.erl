-module(lager_logentries_tcp_backend_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-define(HOST, {127, 0, 0, 1}).

%%==============================================================================
%% CommonTest callbacks
%%==============================================================================

all() ->
    [ sends_logs_over_tcp,
      respects_level,
      handles_reconnections
    ].

init_per_suite(Config) ->
    application:set_env(lager, error_logger_redirect, false),
    lager:start(),
    Config.

end_per_suite(_) ->
    application:stop(lager).

init_per_testcase(_, Config) ->
    %% Open a local socket to emulate Logentries.
    {Socket, Port} = local_socket(),
    Token = "eaa9fd4b-dfd2-469e-b851-3b6519c730d5",
    start_lager_handler(Port, Token),
    RecvSocket = accept(Socket),
    flush(RecvSocket),
    [{socket, Socket}, {recv_socket, RecvSocket}, {port, Port} | Config].

end_per_testcase(_, Config) ->
    stop_lager_handler(?config(port, Config)).

%%==============================================================================
%% Tests Cases
%%==============================================================================

sends_logs_over_tcp(Config) ->
    Log1 = log(info, "hello i am an info log"),
    Log2 = log(critical, "howdy i am a critical log"),

    Logs = flush(?config(recv_socket, Config)),
    assert_logged(Logs, [Log1, Log2]).

respects_level(Config) ->
    Log1 = log(info, "log 1"),
    ok = lager:set_loglevel(handler_id(?config(port, Config)), warning),
    Log2 = log(info, "log 2"),
    Log3 = log(error, "log 3"),

    Logs = flush(?config(recv_socket, Config)),
    assert_logged(Logs, [Log1, Log3]),
    assert_not_logged(Logs, [Log2]).

handles_reconnections(Config) ->
    RecvSocket1 = ?config(recv_socket, Config),

    Log1 = log(info, "log 1"),
    Logs1 = flush(RecvSocket1),
    gen_tcp:close(RecvSocket1),
    Log2 = log(info, "log 2"),
    Log3 = log(info, "log 3"),

    RecvSocket2 = accept(?config(socket, Config)),
    Log4 = log(info, "log 4"),
    Logs2 = flush(RecvSocket2),

    Logs = Logs1 ++ Logs2,
    assert_logged(Logs, [Log1, Log4]),
    %% If socket was down, we drop messages to
    %% avoid saturation of the backend.
    assert_not_logged(Logs, [Log2, Log3]).

%%==============================================================================
%% Private Utilities
%%==============================================================================

-spec start_lager_handler(inet:port_number(), binary()) -> ok.
start_lager_handler(Port, Token) ->
    Opts = [{host, ?HOST}, {port, Port}, {token, Token}],
    ok = gen_event:add_handler(lager_event, handler_id(Port), Opts).

-spec stop_lager_handler(inet:port_number()) -> ok.
stop_lager_handler(Port) ->
    ok = gen_event:delete_handler(lager_event, handler_id(Port), []).

-spec local_socket() -> {gen_tcp:socket(), inet:port_number()}.
local_socket() ->
    {ok, Socket} = gen_tcp:listen(0, [binary,
                                      {ip, ?HOST},
                                      {active, false},
                                      {reuseaddr, true}]),
    {ok, Port} = inet:port(Socket),
    {Socket, Port}.

-spec accept(gen_tcp:socket()) -> gen_tcp:socket().
accept(Socket) ->
    {ok, RecvSocket} = gen_tcp:accept(Socket, 1000),
    RecvSocket.

-spec flush(gen_tcp:socket()) -> [map()].
flush(RecvSocket) ->
    Data = iolist_to_binary(recv(RecvSocket)),
    Logs = binary:split(Data, <<"\r\n">>, [trim_all, global]),
    ct:pal("SPLIT BIN: ~p", [Logs]),
    [jsx:decode(binary:part(Log, {byte_size(Log), -(byte_size(Log)-37)}),
                [return_maps])
     || Log <- Logs].

-spec recv(gen_tcp:socket()) -> binary().
recv(RecvSocket) ->
    recv(RecvSocket, <<>>).

-spec recv(gen_tcp:socket(), iodata()) -> iodata().
recv(RecvSocket, Acc) ->
    case gen_tcp:recv(RecvSocket, 0, 100) of
        {ok, Data} ->
            ct:pal("GOT DATA: ~p", [Data]),
            recv(RecvSocket, [Acc, Data]);
        {error, timeout} ->
            Acc
    end.

-spec handler_id(inet:port_number()) -> term().
handler_id(Port) ->
    {lager_logentries_token_tcp_backend, {?HOST, Port}}.

-spec log(atom(), string()) -> pos_integer().
log(Level, Message) ->
    Ref = erlang:unique_integer([positive, monotonic]),
    lager:log(Level, [{test_log_ref, Ref}], Message),
    Ref.

-spec extract_logrefs([map()]) -> sets:set(integer()).
extract_logrefs(Logs) ->
    Refs = lists:filtermap(fun extract_logref/1, Logs),
    sets:from_list(Refs).

-spec assert_logged([map()], [pos_integer()]) -> ok | no_return().
assert_logged(Logs, ExpectedLogRefs) when is_list(ExpectedLogRefs) ->
    ActualLogRefs = extract_logrefs(Logs),
    lists:foreach( fun (LogRef) ->
                           case sets:is_element(LogRef, ActualLogRefs) of
                               true ->
                                   ok;
                               false ->
                                   error({ log_not_found
                                         , [{log_ref, LogRef}, {logs, Logs}]
                                         })
                           end
                   end
                 , ExpectedLogRefs);
assert_logged(Logs, LogRef) ->
    assert_logged(Logs, [LogRef]).

-spec assert_not_logged([map()], [pos_integer()]) -> ok | no_return().
assert_not_logged(Logs, ExpectedLogRefs) when is_list(ExpectedLogRefs) ->
    ActualLogRefs = extract_logrefs(Logs),
    lists:foreach( fun (LogRef) ->
                           case sets:is_element(LogRef, ActualLogRefs) of
                               false ->
                                   ok;
                               true ->
                                   error({ forbidding_log_found
                                         , [{log_ref, LogRef}, {logs, Logs}]
                                         })
                           end
                   end
                 , ExpectedLogRefs);
assert_not_logged(Logs, LogRef) ->
    assert_not_logged(Logs, [LogRef]).

-spec extract_logref(map()) -> false | {true, pos_integer()}.
extract_logref(#{<<"context">> := #{<<"test_log_ref">> := LogRef}}) ->
    {true, LogRef};
extract_logref(_) ->
    false.
