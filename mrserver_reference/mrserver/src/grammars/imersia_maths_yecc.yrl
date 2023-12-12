Nonterminals exp params.
Terminals str var num bool 'and' 'or' 'not' err lt lte gt gte eq neq '+' '-' '*' '/' '%' lbr rbr ',' func.
Rootsymbol exp.
Endsymbol '$end'.

Expect 2.

Left 300 '+' '-'.
Left 400 '*' '/' '%'.
Left 500 lt lte gt gte eq neq 'and' 'or'.

exp -> num                          : '$1'.
exp -> err                          : '$1'.
exp -> var                          : '$1'.
exp -> str                          : '$1'.

exp -> num num                      : add('$1', '$2').
exp -> exp '+' exp                  : add('$1', '$3').
exp -> exp '-' exp                  : subtract('$1', '$3').
exp -> exp '*' exp                  : multiply('$1', '$3').
exp -> exp '/' exp                  : divide('$1', '$3').
exp -> exp '%' exp                  : domod('$1', '$3').

exp -> bool                         : evaluatebool('$1').
exp -> exp 'and' exp                : doand('$1', '$3').
exp -> exp 'or' exp                 : door('$1', '$3').
exp -> 'not' exp                    : donot('$2').
exp -> exp lt exp                   : dolt('$1', '$3').
exp -> exp gt exp                   : dogt('$1', '$3').
exp -> exp lte exp                  : dolte('$1', '$3').
exp -> exp gte exp                  : dogte('$1', '$3').
exp -> exp eq exp                   : doeq('$1', '$3').
exp -> exp neq exp                  : doneq('$1', '$3').
exp -> lbr exp rbr                  : '$2'.

exp -> func lbr params rbr          : evaluate('$1', '$3').
exp -> func lbr rbr                 : evaluate('$1', []).

params -> exp                       : ['$1'].
params -> exp ',' exp               : ['$1', '$3'].
params -> exp ',' exp ',' exp       : ['$1', '$3', '$5'].

Erlang code.

add({str, LN, Value1}, {str, LN, Value2}) -> {str, LN, <<Value1/binary, Value2/binary>>};
add({_, LN, Value1}, {_, LN, Value2}) -> {num, LN, bool_or_string_to_num(Value1) + bool_or_string_to_num(Value2)}.
subtract({_, LN, Value1}, {_, LN, Value2}) -> {num, LN, bool_or_string_to_num(Value1) - bool_or_string_to_num(Value2)}.
multiply({_, LN, Value1}, {_, LN, Value2}) -> {num, LN, bool_or_string_to_num(Value1) * bool_or_string_to_num(Value2)}.
divide({_, LN, Value1}, {_, LN, Value2}) -> {num, LN, bool_or_string_to_num(Value1) / bool_or_string_to_num(Value2)}.
domod({_, LN, Value1}, {_, LN, Value2}) -> {num, LN, mod(bool_or_string_to_num(Value1), bool_or_string_to_num(Value2))}.
doand({_, LN, Value1}, {_, LN, Value2}) -> {bool, LN, Value1 and Value2}.
door({_, LN, Value1}, {_, LN, Value2}) -> {bool, LN, num_or_string_to_bool(Value1) or num_or_string_to_bool(Value2)}.
donot({_, LN, Value1}) -> {bool, LN, not num_or_string_to_bool(Value1)}.
evaluatebool({_, LN, Value1}) -> {bool, LN, num_or_string_to_bool(Value1)}.
dolt({_, LN, Value1}, {_, LN, Value2}) -> {bool, LN, bool_or_string_to_num(Value1) < bool_or_string_to_num(Value2)}.
dogt({_, LN, Value1}, {_, LN, Value2}) -> {bool, LN, bool_or_string_to_num(Value1) > bool_or_string_to_num(Value2)}.
dolte({_, LN, Value1}, {_, LN, Value2}) -> {bool, LN, bool_or_string_to_num(Value1) =< bool_or_string_to_num(Value2)}.
dogte({_, LN, Value1}, {_, LN, Value2}) -> {bool, LN, bool_or_string_to_num(Value1) >= bool_or_string_to_num(Value2)}.
doeq({_, LN, Value1}, {_, LN, Value2}) -> {bool, LN, Value1 == Value2}.
doneq({_, LN, Value1}, {_, LN, Value2}) -> {bool, LN, Value1 =/= Value2}.

bool_or_string_to_num (Val) when is_binary(Val) -> imersia_misc:convert_number(Val);
bool_or_string_to_num (false) -> 0;
bool_or_string_to_num (true) -> 1;
bool_or_string_to_num (err) -> -1;
bool_or_string_to_num (Val) -> Val.

num_or_string_to_bool(Val) when is_binary(Val) -> imersia_misc:convert_bool(Val);
num_or_string_to_bool (err) -> false;
num_or_string_to_bool (Val) when is_atom (Val) -> Val;
num_or_string_to_bool (Val) when Val >= 1 -> true;
num_or_string_to_bool (_) -> false.

mod(X,Y) when X > 0 -> X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_) -> 0.

evaluate({func, _, <<"if">>}, [{_, _, P1}, {_, _, P2}, {_, _, P3}]) ->
    case num_or_string_to_bool(P1) of
        true -> {num, 1, bool_or_string_to_num(P2)};
        false -> {num, 1, bool_or_string_to_num(P3)}
    end;
evaluate({func, LN, <<"min">>}, [{_, LN, P1}, {_, LN, P2}]) ->
    P1N = bool_or_string_to_num(P1), P2N = bool_or_string_to_num(P2),
    {num, LN, if (P1N < P2N) -> P1N; true -> P2N end};
evaluate({func, LN, <<"max">>}, [{_, LN, P1}, {_, LN, P2}]) ->
    P1N = bool_or_string_to_num(P1), P2N = bool_or_string_to_num(P2),
    {num, LN, if (P1N > P2N) -> P1N; true -> P2N end};
evaluate({func, LN, <<"acos">>}, [{_, LN, P1}]) ->
    {num, LN, math:acos(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"acosh">>}, [{_, LN, P1}]) ->
    {num, LN, math:acosh(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"asin">>}, [{_, LN, P1}]) ->
    {num, LN, math:asin(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"asinh">>}, [{_, LN, P1}]) ->
    {num, LN, math:asinh(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"atan">>}, [{_, LN, P1}]) ->
    {num, LN, math:atan(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"atan2">>}, [{_, LN, P1}, {_, LN, P2}]) ->
    {num, LN, math:atan2(bool_or_string_to_num(P1), bool_or_string_to_num(P2))};
evaluate({func, LN, <<"atanh">>}, [{_, LN, P1}]) ->
    {num, LN, math:atanh(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"ceil">>}, [{_, LN, P1}]) ->
    {num, LN, math:ceil(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"cos">>}, [{_, LN, P1}]) ->
    {num, LN, math:cos(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"cosh">>}, [{_, LN, P1}]) ->
    {num, LN, math:cosh(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"exp">>}, [{_, LN, P1}]) ->
    {num, LN, math:exp(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"floor">>}, [{_, LN, P1}]) ->
    {num, LN, math:floor(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"fmod">>}, [{_, LN, P1}, {_, LN, P2}]) ->
    {num, LN, math:fmod(bool_or_string_to_num(P1), bool_or_string_to_num(P2))};
evaluate({func, LN, <<"log">>}, [{_, LN, P1}]) ->
    {num, LN, math:log(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"log10">>}, [{_, LN, P1}]) ->
    {num, LN, math:log10(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"log2">>}, [{_, LN, P1}]) ->
    {num, LN, math:log2(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"random">>}, []) ->
    {num, LN, rand:uniform()};
evaluate({func, LN, <<"round">>}, [{_, LN, P1}]) ->
    {num, LN, round(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"pow">>}, [{_, LN, P1}, {_, LN, P2}]) ->
    {num, LN, math:pow(bool_or_string_to_num(P1), bool_or_string_to_num(P2))};
evaluate({func, LN, <<"sin">>}, [{_, LN, P1}]) ->
    {num, LN, math:sin(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"sinh">>}, [{_, LN, P1}]) ->
    {num, LN, math:sinh(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"sqrt">>}, [{_, LN, P1}]) ->
    {num, LN, math:sqrt(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"tan">>}, [{_, LN, P1}]) ->
    {num, LN, math:tan(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"tanh">>}, [{_, LN, P1}]) ->
    {num, LN, math:tanh(bool_or_string_to_num(P1))};
evaluate({func, LN, <<"pi">>}, []) ->
    {num, LN, math:pi()};
evaluate(_, _) -> {num, 1, 0}.
