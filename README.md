# lager_logentries

Backend for Lager which sends logs to [Logentries](https://logentries.com)  via [Token TCP](https://docs.logentries.com/docs/input-token)

## Installation

This library is [available on Hex](https://hex.pm/packages/lager_logentries). Just add it to your
Rebar3 dependencies:

```erlang
{deps, [
  {lager_graylog, "0.1.0"}
]}.
```

## Configuration

**Supported options:**

**host:** The hostname of the logentries endpoint. (default: `data.logentries.com`)
**port:** The port number for logentries. (default: `80`)
**address_family:** Defines an specific IP protocol version. `inet` stands for IPv4, `inet6` for IPv6, and `undefined` means that suitable version will be chosen for you automatically. (default: `undefined`)
**token:** The unique logentries token (UUID) for the log destination.
**level:** Minimum level for this backend. (default: `info`)
**context:** Proplist of additional fields to be added when sending the logs. Keys have to be either atoms, binaries or iolists and values must be anything that has an equivalent JSON datatype. (e.g. `[{<<"username">>, <<"codeadict">>}, {<<"environment">>, <<"production">>}]`)

Add configuration to `sys.config`:

```erlang
{lager, [{ handlers
         , [{ lager_logentries_token_tcp_backend
            , [ {host, "data.logentries.com"}
              , {port, 80}
              ]
            }]
         }]
}
```

## Usage

```erlang
-module(example).

-export([main/0]).

main() ->
  lager:info([{service, "mycoolapp"}], "Hello ~s", ["world"]).
```

Would result in JSON like this:

```javascript
{
  "message": "Hello world",
  "timestamp": "2019-02-17T02:40:33.916880",
  "fields": {
    "severity": "info",
    "service": "mycoolapp",
    "application": "example",
    "node": "node@localhost",
    "pid": "<0.13919.0>",
    "module": "example",
    "function": "main",
    "line": "6"
  }
}
```

## Contributing

Pull requests are most welcome. If you have any questions, bug reports or feature proposals just
[open an issue](https://github.com/codeadict/lager_logentries/issues/new).

The project is developed using rebar3:

* run `rebar3 compile` to compile the source code
* run `rebar3 ct` to test it
* run `rebar3 dialyzer` to perform Dialyzer checks

**Note:** Any new piece of code should be reasonably tested and covered by type specs.

## License

Copyright 2019 Dairon Medina Caro

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
