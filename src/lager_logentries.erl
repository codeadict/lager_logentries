-module(lager_logentries).

-export_type([host/0,
              port_number/0,
              mask/0,
              address_family/0,
              json_object/0]).

-type host() :: inet:hostname().
-type port_number() :: inet:port_number().
-type address_family() :: undefined | inet | inet6.

%% log level mask - definition taken from lager
-type mask() :: {mask, integer()}.

-type json_object() :: {atom() | binary(), jsx:json_term()}.
