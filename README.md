# erl_bencode

[![build](https://github.com/thetelefon/erl_bencode/actions/workflows/erlang.yml/badge.svg)](https://github.com/thetelefon/erl_bencode)
[![codecov](https://codecov.io/gh/thetelefon/erl_bencode/branch/master/graph/badge.svg?token=GWYPMBYL96)](https://codecov.io/gh/thetelefon/erl_bencode)
[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://www.apache.org/licenses/LICENSE-2.0)

An OTP library for encoding and decoding bencode.

## Installation

Add erl_bencode to your rebar.config:

```
{deps, [
    erl_bencode
    ]}.
```


## Build

    $ rebar3 compile


## Test

Simply run eunit with rebar3

    $ rebar3 eunit


## Usage

### Example

```
1> bencode:encode("Wardenclyffe").
{ok, "12:Wardenclyffe"}
2> bencode:decode("d6:wardeni23ee").
{ok, #{"warden" => 23}}
3> bencode:decode(valid_file_path).
{ok,#{"a" => "valid","file" => "path","this" => "is"}}
4> 
```

