-module(ecgi).

-export([send/1, recv/1, apply_handler/1, chunked_output/1]).
-export([send_chunked/2]).

send(Data) ->
    send(get(ecgi_output), Data).

send({M,F,A}, Data) ->
    M:F(A, Data);
send({F,A}, Data) ->
    F(A, Data);
send(F, Data) when is_function(F,1) ->
    F(Data).

recv(Length) ->
    recv(get(ecgi_input), Length).

recv({M,F,A}, Length) ->
    M:F(A, Length);
recv({F,A}, Length) ->
    F(A, Length);
recv(F, Length) when is_function(F,1) ->
    F(Length).

apply_handler({M,F,A}) ->
    apply(M,F,A);
apply_handler({F,A}) ->
    apply(F,A);
apply_handler(F) when is_function(F, 0) ->
    F().

chunked_output(Handler) ->
    Output = get(ecgi_output),
    put(ecgi_output, {ecgi, send_chunked, Output}),
    apply_handler(Handler).

send_chunked(Output, Data) ->
    send(Output,
         [erlang:integer_to_binary(iolist_size(Data), 16),
          <<"\r\n">>, Data, <<"\r\n">>]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

send_test() ->
    put(ecgi_output, fun iolist_to_binary/1),
    ?assertEqual(<<"AB">>, send([<<"A">>,<<"B">>])).

recv_test() ->
    put(ecgi_input, fun integer_to_binary/1),
    ?assertEqual(<<"10">>, recv(10)).

send_test_() ->
    [ ?_assertEqual(ok, send({erlang, apply, fun(X) -> X end}, [ok])),
      ?_assertEqual(ok, send({fun (_,X) -> X end, none}, ok)),
      ?_assertEqual(ok, send(fun(X) -> X end, ok)) ].

recv_test_() ->
    [ ?_assertEqual(ok, recv({erlang, apply, fun(X) -> X end}, [ok])),
      ?_assertEqual(ok, recv({fun (_,X) -> X end, none}, ok)),
      ?_assertEqual(ok, recv(fun(X) -> X end, ok)) ].

apply_handler_test_() ->
    Fun = fun() -> ok end,
    [?_assertEqual(ok, apply_handler({erlang, apply, [Fun, []]})),
     ?_assertEqual(ok, apply_handler({Fun, []})),
     ?_assertEqual(ok, apply_handler(Fun))].

chunked_output_test() ->
    put(ecgi_output, fun iolist_to_binary/1),
    ?assertEqual(<<"2\r\nOK\r\n">>, chunked_output(fun () -> ecgi:send(<<"OK">>) end)).

send_chunked_test_() ->
    Send = fun(Data) -> send_chunked(fun iolist_to_binary/1, Data) end,
    [?_assertEqual(<<"2\r\nOK\r\n">>, Send(<<"OK">>)),
     ?_assertEqual(<<"A\r\n0000000000\r\n">>, Send(<<"0000000000">>))].

-endif.
