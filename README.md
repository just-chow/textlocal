# textlocal

[![Build Status](https://travis-ci.org/just-chow/textlocal.svg?branch=master)](https://travis-ci.org/just-chow/textlocal)

Haskell wrapper for sending SMS using textlocal SMS gateway.

## Sending SMS

1. Get an api key from [textlocal.in](http://textlocal.in/)
2. Quick way to send:

``` haskell
import Network.Api.TextLocal
let cred = createUserHash "myemail@email.in" "my-secret-hash"
res <- sendSMS "hello world" ["911234567890"] cred
res
Right (TLResponse {status = Success, warnings = Nothing, errors = Nothing})
```

Or in a more configurable way:

``` haskell
import Network.Api.TextLocal
let mySettings = setTest True defaultSettings
res <- runSettings SendSMS mySettings
res
Right (TLResponse {status = Success, warnings = Nothing, errors = Nothing})
```

