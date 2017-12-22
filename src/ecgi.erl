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
    ?assert(send([<<"A">>,<<"B">>]) =:= <<"AB">>).

recv_test() ->
    put(ecgi_input, fun integer_to_binary/1),
    ?assert(recv(10) =:= <<"10">>).

send_test_() ->
    [ ?_assert(send({erlang, apply, fun(X) -> X end}, [ok]) =:= ok),
      ?_assert(send({fun (_,X) -> X end, none}, ok) =:= ok),
      ?_assert(send(fun(X) -> X end, ok) =:= ok) ].

recv_test_() ->
    [ ?_assert(recv({erlang, apply, fun(X) -> X end}, [ok]) =:= ok),
      ?_assert(recv({fun (_,X) -> X end, none}, ok) =:= ok),
      ?_assert(recv(fun(X) -> X end, ok) =:= ok) ].

apply_handler_test_() ->
    Fun = fun() -> ok end,

    [?_assert(apply_handler({erlang, apply, [Fun, []]}) =:= ok),
     ?_assert(apply_handler({Fun, []}) =:= ok),
     ?_assert(apply_handler(Fun) =:= ok)].

chunked_output_test() ->
    put(ecgi_output, fun iolist_to_binary/1),
    ?assert(chunked_output(fun () -> ecgi:send(<<"OK">>) end) =:= <<"2\r\nOK\r\n">>).

send_chunked_test_() ->
    Send = fun(Data) -> send_chunked(fun iolist_to_binary/1, Data) end,
    [?_assert(Send(<<"OK">>) =:= <<"2\r\nOK\r\n">>),
     ?_assert(Send(<<"0000000000">>) =:= <<"A\r\n0000000000\r\n">>)].
-endif.
