Definitions.

T = true
F = false
E = err
D = [0-9]
A = [a-zA-Z]
H = [0-9a-fA-F]
C = [0-9a-zA-Z_\.]
V = [0-9a-zA-Z_\.\:\[\]]
S = [^\'\"]

Rules.

{H}{H}{H}{H} : {token, {str, 1, list_to_binary(TokenChars)}}.
{H}{H}{H}{H}{H}{H}{H}{H}\-{H}{H}{H}{H}\-{H}{H}{H}{H}\-{H}{H}{H}{H}\-{H}{H}{H}{H}{H}{H}{H}{H}{H}{H}{H}{H}
												: {token, {str, 1, list_to_binary(TokenChars)}}.
{T}                                       		: {token, {bool, 1, true}}.
{F}                                    			: {token, {bool, 1, false}}.
{E}                                    			: {token, {err, 1, err}}.
{A}{C}*\s*\(                                    : {token, {func, 1, list_to_binary(extract_function_name(TokenChars))}, "("}.
{A}{V}*\!                                       : {token, {var, 1, list_to_binary(TokenChars)}}.
{A}{V}*                                         : {token, {var, 1, list_to_binary(TokenChars)}}.
\'{S}+\'										: {token, {str, 1, list_to_binary(string:trim(TokenChars, both, "\'"))}}.
\(                                              : {token, {lbr, 1}}.
\)                                              : {token, {rbr, 1}}.
\+                                              : {token, {'+', 1}}.
\-                                              : {token, {'-', 1}}.
\*                                              : {token, {'*', 1}}.
\/                                              : {token, {'/', 1}}.
\%                                              : {token, {'%', 1}}.
\,                                              : {token, {',', 1}}.
\>												: {token, {gt, 1}}.
\>\=											: {token, {gte, 1}}.
\<												: {token, {lt, 1}}.
\<\=											: {token, {lte, 1}}.
\=\=											: {token, {eq, 1}}.
\!\=											: {token, {neq, 1}}.
\&\&                                            : {token, {'and', 1}}.
\|\|                                            : {token, {'or', 1}}.
\!                                              : {token, {'not', 1}}.
-?{D}+                                          : {token, {num, 1, list_to_integer(TokenChars)}}.
-?{D}+\.{D}+((E|e)(\+|\-)?{D}+)?                : {token, {num, 1, list_to_float(TokenChars)}}.
\s+                                             : skip_token.
\}                                              : {token, {'$end'}}.

Erlang code.

extract_function_name(TokenChars) -> re:replace(TokenChars, "\\s+|\\(", "", [global,{return,list}]).
