-module(ecgi_sse).

-export([send/1]).


normalize_field_name(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
normalize_field_name(List) when is_list(List) ->
    unicode:characters_to_binary(List);
normalize_field_name(Bin) when is_binary(Bin) ->
    Bin.

send(Event) when is_map(Event) ->
    ecgi:send(
      [[ [normalize_field_name(F), <<": ">>, V, <<"\n">>]
         || {F, V} <- maps:to_list(Event)],
       <<"\n">>]);
send(Data) ->
    send(#{<<"data">> => Data}).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_field_name_test_() ->
    Test =
        fun (F,N) ->
                ?_assertEqual(N, normalize_field_name(F))
        end,

    [ Test('event', <<"event">>),
      Test(<<"event">>, <<"event">>),
      Test("event", <<"event">>)
    ].

send_test_() ->
    Send =
        fun (Data) ->
                put(ecgi_output, fun iolist_to_binary/1),
                send(Data)
        end,

     [ ?_assertEqual(<<"data: a\n\n">>, Send(<<"a">>)),
       ?_assertEqual(<<"data: a\nevent: msg\n\n">>, Send(#{event => <<"msg">>, data => <<"a">>})) ].

-endif.
