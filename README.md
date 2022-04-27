# erl_bencode


An OTP library for encoding and decoding bencode.

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

