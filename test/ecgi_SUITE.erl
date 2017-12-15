-module(ecgi_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [eunit].

eunit(_Config) ->
    ok = eunit:test({application, ecgi}, [verbose]).
