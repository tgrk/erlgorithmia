%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Algorithmia API Erlang client - API tests
%%% @end
%%%----------------------------------------------------------------------------
-module(eac_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
eac_server_test_() ->
    {foreach,
     fun () ->
             eac:start()
     end,
     fun (_) ->
             eac:stop()
     end,
     [
       {"Demo Hello Alg", fun test_hello_alg/0}
     , {"Build URI",      fun test_build_uri/0}
     , {"Query params",   fun test_query_params/0}
     , {"Content types",  fun test_content_types/0}
     ]
    }.

%%%=============================================================================
test_hello_alg() ->
    Creds = read_keys(),

    ok = eac:set_api_key(maps:get(<<"api_key">>, Creds)),

    {ok, Map1} = eac:algo(<<"demo/Hello/0.1.1">>, <<"HAL 9000">>, text),
    ?assertEqual(<<"Hello HAL 9000">>, maps:get(<<"result">>, Map1)),
    ?assertEqual(<<"text">>,
                 maps:get(<<"content_type">>, maps:get(<<"metadata">>, Map1))),

    {ok, Map2} = eac:algo(<<"demo/Hello">>, <<"0.1.1">>, <<"HAL 9000">>, text),
    ?assertEqual(<<"Hello HAL 9000">>, maps:get(<<"result">>, Map2)),
    ?assertEqual(<<"text">>,
                 maps:get(<<"content_type">>, maps:get(<<"metadata">>, Map2))),

    {error, Reason} = eac:algo(<<"demo/Hello/0.1.1">>, <<"HAL 9000">>, json),
    ?assertEqual(<<"Failed to parse input, input did not parse as valid json">>,
                 Reason),

    ok.

test_build_uri() ->
    ?assertEqual(<<"https://api.algorithmia.com/v1/algo/foo?stdout=true">>,
                 eac_server:build_uri(<<"foo">>, #{<<"stdout">> => true})),
    ok.

test_query_params() ->
    ValidParams = #{<<"stdout">>  => true,
                    <<"timeout">> => 1,
                    <<"output">>  => raw,
                    <<"foobar">>  => false
              },
    ValidResult = eac_server:sanitize_query_params(ValidParams),
    ?assertEqual(3, maps:size(ValidResult)),

    InvalidParams = #{<<"stdout">>  => foo,
                      <<"timeout">> => <<"foo">>,
                      <<"output">>  => foo,
                      <<"foobar">>  => false
                     },
    InvalidResult = eac_server:sanitize_query_params(InvalidParams),
    ?assertEqual(0, maps:size(InvalidResult)),

    ok.

test_content_types() ->
    ?assertEqual(<<"application/text">>, eac_server:get_content_type(text)),
    ?assertEqual(<<"application/json">>, eac_server:get_content_type(json)),
    ?assertEqual(<<"application/octet-stream">>,
                 eac_server:get_content_type(binary)),
    ok.

%%%=============================================================================
read_keys() ->
    {ok, [Creds]} = file:consult("api_key.txt"),
    Creds.
