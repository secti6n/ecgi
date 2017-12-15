-module(ecgi_handler).

-export([apply/1, handle/2]).


apply({M,F,A}) ->
    apply(M,F,A);
apply({F,A}) ->
    apply(F,A);
apply(F) when is_function(F, 0) ->
    F().


normalize_header_field_name(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
normalize_header_field_name(List) when is_list(List) ->
    unicode:characters_to_binary(List);
normalize_header_field_name(Bin) when is_binary(Bin) ->
    Bin.


handle(Handler, Proto) ->
    {response, {Code, Reason}, Headers, Body} = apply(Handler),

    Response =
        [ Proto, <<" ">>, integer_to_binary(Code), <<" ">>, Reason, <<"\r\n">>,
          [ [normalize_header_field_name(Field), <<": ">>, Value, <<"\r\n">>]
            || {Field, Value} <- maps:to_list(Headers)],
          <<"\r\n">>
        ],

    case Body of
        {M,F,A} ->
            ok = ecgi:send(Response),
            apply(M,F,A);
        {F,A} ->
            ok = ecgi:send(Response),
            apply(F,A);
        F when is_function(F, 0) ->
            ok = ecgi:send(Response),
            F();
        _ ->
            ok = ecgi:send([Response, Body])
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

apply_test_() ->
    Fun = fun() -> ok end,

    [?_assert(apply({erlang, apply, [Fun, []]}) =:= ok),
     ?_assert(apply({Fun, []}) =:= ok),
     ?_assert(apply(Fun) =:= ok)].


normalize_header_field_name_test_() ->
    Test =
        fun (F,N) ->
                ?assert(normalize_header_field_name(F) =:= N)
        end,

    [ ?_test(Test('Connection', <<"Connection">>)),
      ?_test(Test(<<"Connection">>, <<"Connection">>)),
      ?_test(Test("Connection", <<"Connection">>))
    ].

handle_test_() ->
    Test =
        fun (Handler, Response) ->
                Self = self(),
                Ref = make_ref(),
                Send = fun(Data) -> Self ! {Ref, data, Data}, ok end,

                spawn_link(
                  fun () ->
                          put(ecgi_output, Send),
                          handle(Handler, <<"HTTP/1.1">>),
                          Self ! {Ref, eof}
                  end),

                ?assert(
                   iolist_to_binary(
                      (fun Receive() ->
                               receive
                                   {Ref, data, Data} ->
                                       [Data|Receive()];
                                   {Ref, eof} ->
                                       []
                               end
                       end)()) =:= Response)
        end,

    Response =
        <<"HTTP/1.1 200 OK\r\n"
          "Connection: close\r\n"
          "\r\n"
          "OK">>,

    Fun = fun () -> ok = ecgi:send(<<"OK">>) end,
    Status = {200, <<"OK">>},
    Headers = #{'Connection' => <<"close">>},

    [?_test(Test(fun() -> {response, Status, Headers, <<"OK">>} end, Response)),
     ?_test(Test(fun() -> {response, Status, Headers, {erlang, apply, [Fun, []]}} end, Response)),
     ?_test(Test(fun() -> {response, Status, Headers, {Fun, []}} end, Response)),
     ?_test(Test(fun() -> {response, Status, Headers, Fun} end, Response))].
-endif.
