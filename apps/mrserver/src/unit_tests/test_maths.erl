-module(test_maths).

-export([test/0]).

test() ->
    {ok, Tokens, _} = imersia_maths_leex:string("1+2"),
    erlang:display(imersia_maths_yecc:parse(Tokens)),

    {ok, Tokens2, _} = imersia_maths_leex:string("1*2+3/4"),
    erlang:display(imersia_maths_yecc:parse(Tokens2)),

    {ok, Tokens3, _} = imersia_maths_leex:string("1 * 2 + 3 / 4"),
    erlang:display(imersia_maths_yecc:parse(Tokens3)),

    {ok, Tokens4, _} = imersia_maths_leex:string("sin(1)"),
    erlang:display(Tokens4),
    erlang:display(imersia_maths_yecc:parse(Tokens4 ++ [{'$end'}])),

    {ok, Tokens6, _} = imersia_maths_leex:string("(4 > 3)"),
    erlang:display(Tokens6),
    erlang:display(imersia_maths_yecc:parse(Tokens6 ++ [{'$end'}])),

    {ok, all_passed}.
