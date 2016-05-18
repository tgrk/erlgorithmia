# erlgorithmia

[Algorithmia API][2] Erlang client using [Hackney][4] HTTP Client

## Dependencies

* Erlang (>= R18)
* [rebar3][1]


## Running application

`application:start(erlgorithmia)`


## Run locally

```bash
$ ./bin/start.sh
```

## Usage
First you have to obtain `API KEY` for calling API [here][2].

```erlang
1> eac:set_api_key(<<"YOUR API KEY">>).

=INFO REPORT==== 13-May-2016::20:14:34 ===
set_api_key - <<"YOUR API KEY">>
ok
2> eac:algo(<<"demo/Hello/0.1.1">>, <<"HAL 9000">>, text).

=INFO REPORT==== 13-May-2016::20:14:40 ===
[Algorithmia API] URI=https://api.algorithmia.com/v1/algo/demo/Hello/0.1.1
{ok,#{<<"metadata">> => #{<<"content_type">> => <<"text">>,
        <<"duration">> => 0.001761989},
      <<"result">> => <<"Hello HAL 9000">>}}
```

You can pass additional [Hackney HTTP Client options][5]. By default `pool` option
is used.

## Testing

To run all tests use `rebar3 eunit`
To see test coverage `rebar3 cover`

## TODO
* Data API
* better test coverage

[1]: https://github.com/erlang/rebar3
[2]: http://docs.algorithmia.com/?shell#introduction
[3]: https://github.com/inaka/shotgun
[4]: https://github.com/benoitc/hackney/
[5]: https://github.com/benoitc/hackney/blob/master/doc/hackney.md#request-4