
-type host() :: inet:hostname().
-type port_number() :: inet:port_number().
-type address_family() :: undefined | inet | inet6.

%% log level mask - definition taken from lager
-type mask() :: {mask, integer()}.
