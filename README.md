# lager_logentries

Backend for Lager which sends logs to Logentries ( https://logentries.com )

## Installation

This library is [available on Hex](https://hex.pm/packages/lager_logentries). Just add it to your
Rebar3 dependencies:

```erlang
{deps, [
  {lager_graylog, "0.1.0"}
]}.
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
