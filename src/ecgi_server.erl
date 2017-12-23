-module(ecgi_server).

-export(
   [normalize_method/1,
    normalize_request_header_name/1,
    set_header/2,
    handle/2]).

normalize_method(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
normalize_method(Bin) when is_binary(Bin) ->
    Bin.

normalize_request_header_name(Name) ->
    List =
        case Name of
            Atom when is_atom(Atom) -> atom_to_list(Atom);
            Bin when is_binary(Bin) -> unicode:characters_to_list(Bin)
        end,
    Name1 =
        binary:replace(
          unicode:characters_to_binary(
            string:to_upper(List)), <<"-">>, <<"_">>, [global]),
    <<"HTTP_", Name1/binary>>.

set_header(Field, Value) ->
    put(normalize_request_header_name(Field), Value).


normalize_response_header_name(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
normalize_response_header_name(List) when is_list(List) ->
    unicode:characters_to_binary(List);
normalize_response_header_name(Bin) when is_binary(Bin) ->
    Bin.

handle(Handler, Proto) ->
    {response, {Code, Reason}, Headers, Body} = ecgi:apply_handler(Handler),

    Response =
        [ Proto, <<" ">>, integer_to_binary(Code), <<" ">>, Reason, <<"\r\n">>,
          [ [normalize_response_header_name(Field), <<": ">>, Value, <<"\r\n">>]
            || {Field, Value} <- maps:to_list(Headers)],
          <<"\r\n">>
        ],

    case Body of
        {handler, BodyHandler} ->
            ok = ecgi:send(Response),
            ecgi:apply_handler(BodyHandler);
        _ ->
            ok = ecgi:send([Response, Body])
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_method_test_() ->
    Test =
        fun (M,N) ->
                ?_assertEqual(N, normalize_method(M))
        end,

    [ Test('GET', <<"GET">>),
      Test(<<"GET">>, <<"GET">>)
    ].

normalize_request_header_name_test_() ->
    Test =
        fun (F,N) ->
                ?_assertEqual(N, normalize_request_header_name(F))
        end,

    [ Test('Accept', <<"HTTP_ACCEPT">>),
      Test(<<"X-Meta">>, <<"HTTP_X_META">>),
      Test(<<"X_Meta">>, <<"HTTP_X_META">>)
    ].

set_header_test_() ->
    Test =
        fun (F,N) ->
                set_header(F,ok),
                ?assertEqual(ok, get(N))
        end,

    [ ?_test(Test('Accept', <<"HTTP_ACCEPT">>)),
      ?_test(Test(<<"X-Meta">>, <<"HTTP_X_META">>)),
      ?_test(Test(<<"X_Meta">>, <<"HTTP_X_META">>))
    ].


normalize_response_header_name_test_() ->
    Test =
        fun (F,N) ->
                ?_assertEqual(N, normalize_response_header_name(F))
        end,

    [ Test('Connection', <<"Connection">>),
      Test(<<"Connection">>, <<"Connection">>),
      Test("Connection", <<"Connection">>)
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

                ?assertEqual(
                   Response,
                   iolist_to_binary(
                      (fun Receive() ->
                               receive
                                   {Ref, data, Data} ->
                                       [Data|Receive()];
                                   {Ref, eof} ->
                                       []
                               end
                       end)()))
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
     ?_test(Test(fun() -> {response, Status, Headers, {handler, {erlang, apply, [Fun, []]}}} end, Response)),
     ?_test(Test(fun() -> {response, Status, Headers, {handler, {Fun, []}}} end, Response)),
     ?_test(Test(fun() -> {response, Status, Headers, {handler, Fun}} end, Response))].

-endif.
