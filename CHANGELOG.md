

### Version 0.1.5

- Implemented support to decode a erlang term() and not only
decoding from file.
- Optimization of source code.

**Known issues** 
- When encoding and a string consisting of only numbers the translation is a number and not
as expected, a string of numbers

```
"243" will evaluate to i243e

"243" will NOT evaluate to 3:243
```

### Version 0.1.0

First iteration of erl_bencode.

A implementation of both decoding and encoding bencode.
