-module(robots).

-export([parse/2]).

-type agent() :: string() | binary().
-type rule() :: string() | binary().
-type rules() :: sets:set(rule()).
-type content() :: string() | binary().
-type status() :: allowed | disallowed.
-type agent_rules() :: #{agent() := {Allowed :: rules(), Disallowed :: rules()}}.
-type code() :: 400..599.

-spec parse(content(), code()) -> {ok, {status(), all} | {rules, agent_rules()}} | {error, term()}.
parse(_Content, Code) when Code >= 500 andalso Code < 600 ->
    {ok, {disallowed, all}};
parse(_Content, Code) when Code >= 400 ->
    {ok, {allowed, all}};
parse(Content, Code) when Code >= 200 andalso Code < 300 ->
    build_rules(Content);
parse(_Content, Code) ->
    {error, {invalid_status_code, Code}}.

build_rules(_Content) ->
    {ok, {rules, #{}}}.
