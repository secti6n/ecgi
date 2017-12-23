-module(ecgi_client).

-export([request/3, request/4, request/5]).
-export([start_handler/7]).

send({Parent, Ref}, Data) ->
    Parent ! {response_data, Ref, Data},
    ok.

recv(File, Size) when Size > 0 ->
    case file:read(File, Size) of
        eof ->
            {error, closed};
        {ok, Data} when byte_size(Data) < Size ->
            {error, closed};
        Other ->
            Other
    end.

request(Handler, Method, Path) ->
    request(Handler, Method, Path, []).

request(Handler, Method, Path, Options) ->
    request(Handler, Method, Path, #{}, Options).

request(Handler, Method, Path, Headers, Options) ->
    Ref = make_ref(),
    spawn_link(
      ?MODULE, start_handler,
      [self(), Ref, Handler, Method, Path, Headers, proplists:delete(no_wait, Options)]),

    case proplists:get_bool(no_wait, Options) of
        true ->
            Ref;
        false ->
            wait_response(Ref)
    end.

wait_response(Ref) ->
    receive
        {response_start, Ref, Status, Headers} ->
            ok
    end,
    Body = wait_response_body(Ref),
    {response, Status, Headers, iolist_to_binary(Body)}.

wait_response_body(Ref) ->
    receive
        {response_end, Ref} ->
            [];
        {response_data, Ref, Data} ->
            [Data|wait_response_body(Ref)]
    end.

start_handler(Parent, Ref, Handler, Method, Path, Headers, Options) ->
    put(<<"REQUEST_METHOD">>, ecgi_server:normalize_method(Method)),
    put(<<"REQUEST_URI">>, Path),
    [ ecgi_server:set_header(Field, Value)
      || {Field, Value} <- maps:to_list(Headers) ],

    Input =
        case proplists:lookup(data, Options) of
            {data, Data} ->
                {ok, File} = file:open(Data, [read, binary, ram]),
                {fd, File};
            none ->
                case proplists:lookup(file, Options) of
                    {file, Name} ->
                        {ok, File} = file:open(Name, [read, binary]),
                        {fd, File};
                    none ->
                        proplists:lookup(input, Options)
                end
        end,
    handle(Parent, Ref, Handler, Input).


handle(Parent, Ref, Handler, {fd, File}) ->
    try
        handle(Parent, Ref, Handler, {input, {fun recv/2, File}})
    after
        file:close(File)
    end;
handle(Parent, Ref, Handler, {input, Input}) ->
    put(ecgi_input, Input),
    handle(Parent, Ref, Handler, none);
handle(Parent, Ref, Handler, none) ->
    put(ecgi_output, {fun send/2, {Parent, Ref}}),

    {response, Status, Headers, Body} = ecgi:apply_handler(Handler),
    Parent ! {response_start, Ref, Status, Headers},
    case Body of
        {handler, BodyHandler} ->
            ecgi:apply_handler(BodyHandler);
        _ ->
            ok = ecgi:send(Body)
    end,

    Parent ! {response_end, Ref}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

request_test_() ->
    Status = {200, <<"OK">>},
    Headers = #{<<"Connection">> => <<"close">>},
    Method = <<"GET">>,
    Path = <<"/">>,

    Handler =
        fun(Response) ->
                fun () ->
                        Method = get(<<"REQUEST_METHOD">>),
                        Path = get(<<"REQUEST_URI">>),
                        Response
                end
        end,

    Response = {response, Status, Headers, <<"OK">>},
    Fun = fun () -> ok = ecgi:send(<<"OK">>) end,

    [ ?_assertEqual(Response, request(Handler(Response), Method, Path)),
      ?_assertEqual(Response, request(Handler({response, Status, Headers, {handler, {erlang, apply, [Fun, []]}}}), Method, Path)),
      ?_assertEqual(Response, request(Handler({response, Status, Headers, {handler, {Fun, []}}}), Method, Path)),
      ?_assertEqual(Response, request(Handler({response, Status, Headers, {handler, Fun}}), Method, Path)),
      ?_assertEqual(Response, wait_response(request(Handler(Response), Method, Path, [no_wait])))
    ].

request_body_test() ->
    Status = {200, <<"OK">>},
    Path = <<"/">>,
    Response = {response, Status, #{}, <<"OK">>},

    Handler =
        fun () ->
                {ok, Bin} = ecgi:recv(2),
                {response, Status, #{}, Bin}
        end,

    ?assertEqual(Response, request(Handler, 'PUT', Path, [{data, <<"OK">>}])).

request_body_too_short_test_() ->
    Status = {200, <<"OK">>},
    Path = <<"/">>,
    Response = {response, Status, #{}, <<"OK">>},

    Handler =
        fun () ->
                {error, closed} = ecgi:recv(2),
                {response, Status, #{}, <<"OK">>}
        end,

    [?_assertEqual(Response, request(Handler, 'PUT', Path, [{data, <<"">>}])),
     ?_assertEqual(Response, request(Handler, 'PUT', Path, [{data, <<"O">>}]))].

-endif.
