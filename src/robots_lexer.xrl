Definitions.

Newline = [\r\n][\n]?
Characters = (.)+
Whitespaces = [\s\t]
StartGroupLine = (u|U)(s|S)(e|E)(r|R)(-)(a|A)(g|G)(e|E)(n|N)(t|T)
SiteMap = (s|S)(i|I)(t|T)(e|E)(m|M)(a|A)(p|P)
Allow = (a|A)(l|L)(l|L)(o|O)(w|W)
Disallow = (d|D)(i|I)(s|S)(a|A)(l|L)(l|L)(o|O)(w|W)
Comment = (#).*
Wildcard = \*

Rules.

{Whitespaces}*{StartGroupLine}{Whitespaces}* : {token, {user_agent, TokenLine}}.
{Whitespaces}*{Allow}{Whitespaces}*          : {token, {allow, TokenLine}}.
{Whitespaces}*{Disallow}{Whitespaces}*       : {token, {disallow, TokenLine}}.
{Whitespaces}*{SiteMap}{Whitespaces}*        : {token, {sitemap, TokenLine}}.
{Whitespaces}*{Comment}{Whitespaces}*        : {token, {comment, TokenLine}}.
\:{Whitespaces}*{Wildcard}{Whitespaces}*     : {token, {wildcard, TokenLine}}.
\:{Whitespaces}*{Characters}+{Whitespaces}*  : {token, {characters, TokenLine, remove_semi_colon(TokenChars)}}.
{Newline}                                    : {token, {newline, TokenLine}}.
.                                            : {token, {unknown, TokenLine, TokenChars}}.

Erlang code.

remove_semi_colon(Characters) ->
    Length = string:len(Characters),
    string:substr(Characters, 1, Length).
