-module(lager_logentries_formatter).

-export([format/2]).

-spec format(lager_msg:lager_msg(), [lager_logentries:json_object()]) ->
                    jsx:json_term().
format(Message, Context) ->
    Metadata = lager_msg:metadata(Message),
    ExtraContext = lager_logentries_utils:safe_json(Metadata) ++ Context,
    #{time => iso_datetime(lager_msg:timestamp(Message)),
      level => lager_msg:severity(Message),
      message => list_to_binary(lager_msg:message(Message)),
      context => ExtraContext
     }.

iso_datetime({_, _, Micro} = Ts) ->
    {UTCDate, {H, M, S}} = calendar:now_to_universal_time(Ts),
    UTCTime = {H, M, S, Micro div 1000 rem 1000},
    {RawDate, RawTime} = lager_util:format_time({UTCDate, UTCTime}),
    iolist_to_binary(io_lib:format("~sT~sZ", [RawDate, RawTime])).
