-module(robots).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([parse/2,
         match/2,
         is_allowed/3]).

-export_type([agent_rules/0]).

-type code() :: 400..599.
-type agent() :: binary().
-type rule() :: binary().
-type rules() :: [rule()].
-type content() :: string() | binary().
-type status() :: allowed | disallowed.
-type rules_index() :: #{agent() := {Allowed :: rules(), Disallowed :: rules()}}.
-opaque agent_rules() :: {status(), all} | rules_index().

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(content(), code()) -> {ok, agent_rules()} | {error, term()}.
parse(_Content, Code) when Code >= 500 andalso Code < 600 ->
    {ok, {disallowed, all}};
parse(_Content, Code) when Code >= 400 ->
    {ok, {allowed, all}};
parse(Content, Code) when Code >= 200 andalso Code < 300 ->
    build_rules(Content);
parse(_Content, Code) ->
    {error, {invalid_status_code, Code}}.

-spec is_allowed(agent(), uri_string:uri_string(), agent_rules()) -> boolean().
is_allowed(_Agent, _Url, {allowed, all}) ->
    true;
is_allowed(_Agent, _Url, {disallowed, all}) ->
    false;
is_allowed(_Agent, _Url, _Rules) ->
    undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================

build_rules(Content) when is_list(Content) ->
    Binary = unicode:characters_to_binary(Content),
    build_rules(Binary);
build_rules(Content) ->
    Split = string:lexemes(Content, [[$\r, $\n], $\r, $\n]),
    Sanitized = lists:filtermap(fun sanitize/1, Split),
    lists:foldl(fun build_rules/2, {[], false, #{}}, Sanitized).

sanitize(Line) ->
    Trimmed = trim(Line),
    case string:take(Trimmed, [$#], true) of
        {<<>>, _} -> false;
        {NotComment, _} ->
            Split = string:split(NotComment, ":"),
            [Key, Agent | _] = lists:map(fun trim/1, Split),
            {true, {string:lowercase(Key), Agent}}
    end.

-spec trim(unicode:chardata()) -> unicode:chardata().
trim(String) ->
    string:trim(String, both).

build_rules({<<"user-agent">>, Agent}, {Agents, false, RulesIndex}) ->
    {[Agent | Agents], false, RulesIndex};
build_rules({<<"user-agent">>, Agent}, {Agents, true, RulesIndex}) ->
    {[Agent | Agents], false, RulesIndex};
build_rules({<<"allow">>, Rule}, {Agents, _, RulesIndex}) ->
    UpdatedIndex = lists:foldl(fun update_index/2, {{allowed, Rule}, RulesIndex}, Agents),
    {Agents, true, UpdatedIndex};
build_rules({<<"disallow">>, Rule}, {Agents, _, RulesIndex}) ->
    UpdatedIndex = lists:foldl(fun update_index/2, {{disallowed, Rule}, RulesIndex}, Agents),
    {Agents, true, UpdatedIndex}.

update_index(Agent, {{allowed, Rule}, RulesIndex}) ->
    Update = fun ({Allowed, Disallowed}) -> {[Rule | Allowed], Disallowed} end,
    UpdatedIndex = maps:update_with(Agent, Update, {[Rule], []}, RulesIndex),
    {{allowed, Rule}, UpdatedIndex};
update_index(Agent, {{disallowed, Rule}, RulesIndex}) ->
    Update = fun ({Allowed, Disallowed}) -> {Allowed, [Rule | Disallowed]} end,
    UpdatedIndex = maps:update_with(Agent, Update, {[], [Rule]}, RulesIndex),
    {{disallowed, Rule}, UpdatedIndex}.

-spec match(binary(), rule()) -> boolean().
match(<<>>, <<$$>>) ->
    true;
match(_, <<$$>>) ->
    false;
match(_, <<$*>>) ->
    true;
match(<<$/, _/binary>>, <<$/>>) ->
    true;
match(_, <<$/>>) ->
    false;
match(<<>>, <<>>) ->
    true;
match(<<>>, _) ->
    false;
match(_, <<>>) ->
    true;
match(<<A, R1/binary>>, <<$*, A, R2/binary>>) ->
    match(R1, R2);
match(<<_, R1/binary>>, <<$*, _, _/binary>>=R2) ->
    match(R1, R2);
match(<<A, R1/binary>>, <<A, R2/binary>>) ->
    match(R1, R2);
match(<<_, _/binary>>, <<_, _/binary>>) ->
    false.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-ifdef(TEST).
simple_path_test_() ->
    Rule = <<"/fish">>,
    [
     ?_assert(match(<<"/fish">>, Rule)),
     ?_assert(match(<<"/fish.html">>, Rule)),
     ?_assert(match(<<"/fish/salmon.html">>, Rule)),
     ?_assert(match(<<"/fishheads">>, Rule)),
     ?_assert(match(<<"/fishheads/yummy.html">>, Rule)),
     ?_assert(match(<<"/fish.php?id=anything">>, Rule)),

     ?_assertNot(match(<<"/Fish.asp">>, Rule)),
     ?_assertNot(match(<<"/catfish">>, Rule)),
     ?_assertNot(match(<<"/?id=fish">>, Rule))
    ].

trailing_wildcard_test_() ->
    Rule = <<"/fish*">>,
    [
     ?_assert(match(<<"/fish">>, Rule)),
     ?_assert(match(<<"/fish.html">>, Rule)),
     ?_assert(match(<<"/fish/salmon.html">>, Rule)),
     ?_assert(match(<<"/fishheads">>, Rule)),
     ?_assert(match(<<"/fishheads/yummy.html">>, Rule)),
     ?_assert(match(<<"/fish.php?id=anything">>, Rule)),

     ?_assertNot(match(<<"/Fish.asp">>, Rule)),
     ?_assertNot(match(<<"/catfish">>, Rule)),
     ?_assertNot(match(<<"/?id=fish">>, Rule))
    ].

trailing_slash_test_() ->
    Rule = <<"/fish/">>,
    [
     ?_assert(match(<<"/fish/">>, Rule)),
     ?_assert(match(<<"/fish/?id=anything">>, Rule)),
     ?_assert(match(<<"/fish/salmon.htm">>, Rule)),

     ?_assertNot(match(<<"/fish">>, Rule)),
     ?_assertNot(match(<<"/fish.html">>, Rule)),
     ?_assertNot(match(<<"/Fish/Salmon.asp">>, Rule))
    ].

nested_wildcard_test_() ->
    Rule = <<"/*.php">>,
    [
     ?_assert(match(<<"/filename.php">>, Rule)),
     ?_assert(match(<<"/folder/filename.php">>, Rule)),
     ?_assert(match(<<"/folder/filename.php?parameters">>, Rule)),
     ?_assert(match(<<"/folder/any.php.file.html">>, Rule)),
     ?_assert(match(<<"/filename.php/">>, Rule)),

     ?_assertNot(match(<<"/">>, Rule)),
     ?_assertNot(match(<<"/windows.PHP">>, Rule))
    ].

nested_wilcard_with_ending_test_() ->
    Rule = <<"/*.php$">>,
    [
     ?_assert(match(<<"/filename.php">>, Rule)),
     ?_assert(match(<<"/folder/filename.php">>, Rule)),

     ?_assertNot(match(<<"/filename.php?parameters">>, Rule)),
     ?_assertNot(match(<<"/filename.php/">>, Rule)),
     ?_assertNot(match(<<"/filename.php5">>, Rule)),
     ?_assertNot(match(<<"/windows.PHP">>, Rule))
    ].

simple_path_with_nested_wildcard_test_() ->
    Rule = <<"/fish*.php">>,
    [
     ?_assert(match(<<"/fish.php">>, Rule)),
     ?_assert(match(<<"/fishheads/catfish.php?parameters">>, Rule)),

     ?_assertNot(match(<<"/Fish.PHP">>, Rule))
    ].
-endif.
