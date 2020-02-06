Header "%% Copyright (C)"
       "%% @private"
       "%% @author Antoine Gagné".

Nonterminals
rules rule rsitemap rdisallow rallow ruser_agent rcomment.

Terminals
user_agent allow disallow sitemap comment wildcard characters newline.

Rootsymbol rules.

rules -> rule rules : ['$1' | '$2'].
rules -> rule : ['$1'].

rule -> ruser_agent : '$1'.
rule -> rallow      : '$1'.
rule -> rdisallow   : '$1'.
rule -> rsitemap    : '$1'.
rule -> rcomment    : '$1'.

ruser_agent -> user_agent characters comment newline : {user_agent, extract('$2')}.
ruser_agent -> user_agent characters newline         : {user_agent, extract('$2')}.
ruser_agent -> user_agent wildcard comment newline   : {user_agent, '*'}.
ruser_agent -> user_agent wildcard newline           : {user_agent, '*'}.

rallow -> allow characters comment newline : {allow, extract('$2')}.
rallow -> allow characters newline         : {allow, extract('$2')}.
rallow -> allow wildcard comment newline   : {allow, '*'}.
rallow -> allow wildcard newline           : {allow, '*'}.

rdisallow -> disallow characters comment newline : {disallow, extract('$2')}.
rdisallow -> disallow characters newline         : {disallow, extract('$2')}.
rdisallow -> disallow wildcard comment newline   : {disallow, '*'}.
rdisallow -> disallow wildcard newline           : {disallow, '*'}.

rsitemap -> sitemap characters comment newline : {sitemap, extract('$2')}.
rsitemap -> sitemap characters newline         : {sitemap, extract('$2')}.

rcomment -> comment newline : comment.

Erlang code.

extract({_, _, Value}) ->
    Value.
