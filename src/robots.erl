%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @copyright 2019 Antoine Gagné
%% @doc Parse and manipulate robots.txt files according to the specification.
-module(robots).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([parse/2,
         sitemap/1,
         is_allowed/3]).

-export_type([agent_rules/0]).

-type code() :: 100..599.
-type agent() :: binary().
-type rule() :: binary().
-type rules() :: [rule()].
-type content() :: string() | binary().
-type status() :: allowed | disallowed.
-type allowed_all() :: {allowed, all}.
-type rules_index() :: #{agent() := {Allowed :: rules(), Disallowed :: rules()} | allowed_all(),
                         sitemap => binary()}.
-type sitemap() :: binary().
-opaque agent_rules() :: {status(), all} | rules_index().

-define(ALL, <<"*">>).

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(content(), code()) -> {ok, agent_rules()} | {error, term()}.
%% @doc Parses the content of the <em>robot.txt</em> and returns all the rules
%%      indexed by their agents.
parse(_Content, Code) when Code >= 500 andalso Code < 600 ->
    {ok, {disallowed, all}};
parse(_Content, Code) when Code >= 400 ->
    {ok, {allowed, all}};
parse(Content, Code) when Code >= 200 andalso Code < 300 ->
    build_rules(Content);
parse(_Content, Code) ->
    {error, {invalid_status_code, Code}}.

-spec is_allowed(agent(), uri_string:uri_string(), agent_rules()) -> boolean().
%% @doc Verifies that the given URL is allowed for the specified agent.
is_allowed(_Agent, _Url, {allowed, all}) ->
    true;
is_allowed(_Agent, _Url, {disallowed, all}) ->
    false;
is_allowed(Agent, Url, RulesIndex) ->
    MaybeRules = find_agent_rules(Agent, RulesIndex),
    is_allowed(Url, MaybeRules).

-spec sitemap(agent_rules()) -> {ok, sitemap()} | {error, not_found}.
%% @doc Fetches the sitemap of the parsed index.
sitemap(RulesIndex) ->
    case maps:find(sitemap, RulesIndex) of
        error -> {error, not_found};
        V={ok, _} -> V
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec find_agent_rules(binary(), agent_rules()) ->
    {error, not_found} | {ok, {rules(), rules()} | allowed_all()}.
find_agent_rules(<<>>, RulesIndex) ->
    case maps:find(?ALL, RulesIndex) of
        error -> {error, not_found};
        Result -> Result
    end;
find_agent_rules(Agent, RulesIndex) ->
    case maps:find(Agent, RulesIndex) of
        Result={ok, _} -> Result;
        error ->
            Size = byte_size(Agent),
            find_agent_rules(binary:part(Agent, 0, Size - 1), RulesIndex)
    end.

-spec is_allowed(binary(), {ok, {rules(), rules()} | allowed_all()} | {error, term()}) -> boolean().
is_allowed(_Url, {ok, {allowed, all}}) ->
    true;
is_allowed(Url, {ok, {Allowed, Disallowed}}) ->
    Match = fun (Rule) -> match(Url, Rule) end,
    lists:any(Match, Allowed) orelse not lists:any(Match, Disallowed);
is_allowed(_Url, {error, _}) ->
    true.

-spec build_rules(binary() | string()) -> {ok, rules_index()}.
build_rules(Content) when is_list(Content) ->
    Binary = unicode:characters_to_binary(Content),
    build_rules(Binary);
build_rules(Content) ->
    Split = string:lexemes(Content, [[$\r, $\n], $\r, $\n]),
    Sanitized = lists:filtermap(fun sanitize/1, Split),
    WithEof = Sanitized ++ [{<<"eof">>, <<"end">>}],
    {_, _, Rules} = lists:foldl(fun build_rules/2, {[], false, #{}}, WithEof),
    {ok, maps:map(fun sort_rules/2, Rules)}.

-spec sanitize(binary()) -> false | {true, {binary(), binary()}}.
sanitize(Line) ->
    Trimmed = trim(Line),
    case string:take(Trimmed, [$#], true) of
        {<<>>, _} -> false;
        {NotComment, _} -> handle_line(NotComment)
    end.

-spec handle_line(binary()) -> {true, {binary(), binary()}} | false.
handle_line(Line) ->
  case string:split(Line, ":") of
      Split=[_, _ | _] ->
          [Key, Value | _] = lists:map(fun trim/1, Split),
          {true, {string:lowercase(Key), Value}};
      _ ->
          false
  end.

-spec sort_rules(agent() | sitemap, {[rule()], [rule()]} | allowed_all() | binary()) ->
    binary() | {[rule()], [rule()]}.
sort_rules(_, Value={allowed, all}) ->
    Value;
sort_rules(_, {Allowed, Disallowed}) ->
    Compare = fun (R1, R2) -> not (R1 =< R2) end,
    {lists:sort(Compare, Allowed), lists:sort(Compare, Disallowed)};
sort_rules(sitemap, Value) ->
    Value.

-spec trim(unicode:chardata()) -> unicode:chardata().
trim(String) ->
    string:trim(String, both).

-spec build_rules({binary(), binary()}, {[agent()], boolean(), rules_index()}) ->
    {[agent()], boolean(), rules_index()}.
build_rules({<<"user-agent">>, Agent}, {Agents, false, RulesIndex}) ->
    {[Agent | Agents], false, RulesIndex};
build_rules({<<"user-agent">>, Agent}, {_Agents, true, RulesIndex}) ->
    {[Agent], false, RulesIndex};
build_rules({<<"allow">>, Rule}, {Agents, _, RulesIndex}) ->
    {_, UpdatedIndex} = lists:foldl(fun update_index/2, {{allowed, Rule}, RulesIndex}, Agents),
    {Agents, true, UpdatedIndex};
build_rules({<<"disallow">>, Rule}, {Agents, _, RulesIndex}) ->
    {_, UpdatedIndex} = lists:foldl(fun update_index/2, {{disallowed, Rule}, RulesIndex}, Agents),
    {Agents, true, UpdatedIndex};
build_rules({<<"eof">>, _}, {Agents, false, RulesIndex}) ->
    {_, UpdatedIndex} = lists:foldl(fun update_index/2, {{allowed, all}, RulesIndex}, Agents),
    {Agents, false, UpdatedIndex};
build_rules({<<"sitemap">>, Map}, {Agents, ParsingRules, RulesIndex}) ->
    {Agents, ParsingRules, RulesIndex#{sitemap => Map}};
build_rules({_Invalid, _Rule}, Acc) ->
    Acc.

-spec update_index(agent(), {{status(), rule()}, rules_index()}) ->
    {{status(), rule()}, rules_index()}.
update_index(Agent, {Rule={allowed, all}, RulesIndex}) ->
    Update = fun (_) -> Rule end,
    UpdatedIndex = maps:update_with(Agent, Update, Rule, RulesIndex),
    {Rule, UpdatedIndex};
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
%%% EUnit Tests
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

user_agent_matching_test_() ->
    News = <<"/news">>,
    All = <<"/all">>,
    Generic = <<"/generic">>,
    RulesIndex = #{<<"googlebot-news">> => {[News], []},
                   <<"*">> => {[All], []},
                   <<"googlebot">> => {[Generic], []}},
    [
     ?_assertMatch({ok, {[News], []}}, find_agent_rules(<<"googlebot-news/1.0.0">>, RulesIndex)),
     ?_assertMatch({ok, {[Generic], []}}, find_agent_rules(<<"googlebot-web*">>, RulesIndex)),
     ?_assertMatch({ok, {[Generic], []}}, find_agent_rules(<<"googlebot-images*">>, RulesIndex)),
     ?_assertMatch({ok, {[All], []}}, find_agent_rules(<<"otherbot-web/1.2.0">>, RulesIndex)),
     ?_assertMatch({ok, {[All], []}}, find_agent_rules(<<"otherbot-news/1.2.0">>, RulesIndex)),

     ?_assertMatch({error, not_found}, find_agent_rules(<<"non-existent/1.0.0">>, #{}))
    ].
-endif.
