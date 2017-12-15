-module(ecgi_server).

-export(
   [normalize_method/1,
    normalize_header_field_name/1,
    set_header/2]).

normalize_method(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
normalize_method(Bin) when is_binary(Bin) ->
    Bin.

normalize_header_field_name(Name) ->
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
    put(normalize_header_field_name(Field), Value).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

normalize_method_test_() ->
    Test =
        fun (M,N) ->
                ?assert(normalize_method(M) =:= N)
        end,

    [ ?_test(Test('GET', <<"GET">>)),
      ?_test(Test(<<"GET">>, <<"GET">>))
    ].

normalize_header_field_name_test_() ->
    Test =
        fun (F,N) ->
                ?assert(normalize_header_field_name(F) =:= N)
        end,

    [ ?_test(Test('Accept', <<"HTTP_ACCEPT">>)),
      ?_test(Test(<<"X-Meta">>, <<"HTTP_X_META">>)),
      ?_test(Test(<<"X_Meta">>, <<"HTTP_X_META">>))
    ].

set_header_test_() ->
    Test =
        fun (F,N) ->
                set_header(F,ok),
                ?assert(get(N) =:= ok)
        end,

    [ ?_test(Test('Accept', <<"HTTP_ACCEPT">>)),
      ?_test(Test(<<"X-Meta">>, <<"HTTP_X_META">>)),
      ?_test(Test(<<"X_Meta">>, <<"HTTP_X_META">>))
    ].
-endif.
