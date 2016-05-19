%%%----------------------------------------------------------------------------
%%% @author Martin Wiso <martin@wiso.cz>
%%% @doc
%%% Algorithmia API Erlang client - API tests with keys
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
     %% , {"Build URI",      fun test_build_uri/0}
     %% , {"Query params",   fun test_query_params/0}
     %% , {"Content types",  fun test_content_types/0}
     %% , {"HTTP options",   fun test_http_options/0}
     ]
    }.

%%%=============================================================================
test_hello_alg() ->
    ok = meck:new(hackney, [unstick, passthrough]),
    try
        ok = mock_request(<<"demo/Hello/0.1.1">>, valid),

        {ok, Map} = eac:algo(<<"demo/Hello">>, <<"0.1.1">>, <<"HAL 9000">>, text),
        ?assertEqual(<<"Hello HAL 9000">>, maps:get(<<"result">>, Map)),
        ?assertEqual(<<"text">>, maps:get(<<"content_type">>,
                                          maps:get(<<"metadata">>, Map)))

    after
        ok = meck:unload(hackney)
    end.

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

test_http_options() ->
    Options = [{pool,            default},
               {follow_redirect, boolean}],

    ?assertEqual([hd(Options)], eac:get_http_options()),

    ?assertEqual(ok, eac:set_http_options(Options)),
    ?assertEqual(Options, eac:get_http_options()),
    ok.

%%%=============================================================================
mock_request(UriPath, ResponseType) ->
    meck:expect(hackney, request,
                fun(Method, Uri, _Headers, _Body, Options) ->
                        ?assertEqual(post, Method),
                        ?assertEqual(eac_server:build_uri(UriPath, #{}), Uri),
                        ?assertEqual([{pool, default}], Options),
                        {ok, 200, [], connection1}
                end),
    meck:expect(hackney, body,
                fun(connection1) ->
                        {ok, jiffy:encode(get_response(ResponseType))}
                end),
    meck:expect(hackney, close, fun(connection2) -> ok end).



get_response(error) ->
    #{<<"error">> =>
          #{<<"message">> =>
                <<"Failed to parse input, input did not parse as valid json">>}
     };
get_response(valid) ->
    #{<<"metadata">> =>
          #{<<"content_type">> => <<"text">>,
            <<"duration">> => 2.37616e-4},
      <<"result">> => <<"Hello HAL 9000">>
     }.
