-module(robots_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(UNSUPPORTED_CODE, 109).
-define(CODE_4XX, 418).
-define(CODE_5XX, 514).
-define(EMPTY_CONTENT, <<>>).
-define(USER_AGENT, <<"bot/1.0.0">>).
-define(ANOTHER_USER_AGENT, <<"BoT/1.0.0">>).
-define(REVERSED_USER_AGENT, <<"0.0.1/tob">>).
-define(NON_EXISTENT_USER_AGENT, <<"nonexistent/1.0.0">>).
-define(AN_URL, <<"/bot-url">>).
-define(A_MATCHING_URL, <<"/foo/">>).
-define(ANOTHER_MATCHING_URL, <<"/bar">>).
-define(A_RULE, <<"/foo/*">>).
-define(ANOTHER_RULE, <<"/bar">>).
-define(A_VALID_CODE, 200).
-define(A_VALID_CONTENT, <<"User-Agent: ", ?USER_AGENT/binary, "\nAllow: ", ?A_RULE/binary>>).
-define(ANOTHER_VALID_CONTENT,
    <<"User-Agent: ", ?USER_AGENT/binary, "\nAllow: ", ?A_RULE/binary, "\nDisallow: ",
        ?ANOTHER_RULE/binary>>
).
-define(SOME_CONTENT_WITH_REPEATED_AGENTS,
    <<"User-Agent: ", ?USER_AGENT/binary, "\nAllow: ", ?A_RULE/binary, "\nUser-Agent: ",
        ?ANOTHER_USER_AGENT/binary, "\nDisallow: ", ?ANOTHER_RULE/binary>>
).

-define(A_VALID_CONTENT_WITH_COMMENT, <<?A_VALID_CONTENT/binary, "# this is a comment">>).
-define(A_MALFORMED_CONTENT, <<"User-Agent: ", ?USER_AGENT/binary, "\n", ?A_RULE/binary>>).
-define(SITEMAP, <<"http://somesitemap.com/map.xml">>).
-define(CONTENT_WITH_SITEMAP, <<"Sitemap:", ?SITEMAP/binary>>).

all() ->
    [{group, all}].

groups() ->
    [
        {all, [parallel], [
            return_error_on_unsupported_status_code,
            allow_all_on_4xx_code,
            disallow_all_on_5xx,
            return_true_if_everything_is_allowed,
            return_false_if_everything_is_disallowed,
            can_parse_valid_robots_txt,
            can_parse_valid_non_binary_robots_txt,
            can_handle_malformed_content,
            merge_repeated_agent,
            can_fetch_sitemap,
            return_error_on_non_existent_sitemap,
            allow_all_on_unmatched_agents_at_end_of_file,
            ignore_inline_comments,
            return_true_if_agent_is_allowed,
            match_independently_of_the_casing_of_the_agent,
            return_false_if_agent_is_disallowed,
            return_true_if_no_matching_rules_can_be_found,
            return_true_if_everything_is_allowed_for_the_corresponding_agent
        ]}
    ].

return_error_on_unsupported_status_code() ->
    [{doc, "Given an unsupported status code, when parsing, then returns an error."}].

return_error_on_unsupported_status_code(_Config) ->
    ?assertMatch({error, _}, robots:parse(?EMPTY_CONTENT, ?UNSUPPORTED_CODE)).

allow_all_on_4xx_code() ->
    [{doc, "Given a 4XX status code, when parsing, then returns all allowed."}].

allow_all_on_4xx_code(_Config) ->
    ?assertMatch({ok, {allowed, all}}, robots:parse(?EMPTY_CONTENT, ?CODE_4XX)).

disallow_all_on_5xx() ->
    [{doc, "Given a 5XX status code, when parsing, then returns all disallowed."}].

disallow_all_on_5xx(_Config) ->
    ?assertMatch({ok, {disallowed, all}}, robots:parse(?EMPTY_CONTENT, ?CODE_5XX)).

return_true_if_everything_is_allowed() ->
    [
        {doc,
            "Given a set of rules that specifies that everything is allowed, "
            "when checking if allowed, then returns true."}
    ].

return_true_if_everything_is_allowed(_Config) ->
    ?assert(robots:is_allowed(?USER_AGENT, ?AN_URL, {allowed, all})).

return_false_if_everything_is_disallowed() ->
    [
        {doc,
            "Given a set of rules that specifies that everything is allowed, "
            "when checking if allowed, then returns false."}
    ].

return_false_if_everything_is_disallowed(_Config) ->
    ?assertNot(robots:is_allowed(?USER_AGENT, ?AN_URL, {disallowed, all})).

can_parse_valid_robots_txt() ->
    [{doc, "Given a valid robots.txt content, when parsing, then returns all rules."}].

can_parse_valid_robots_txt(_Config) ->
    ?assertMatch(
        {ok, #{?REVERSED_USER_AGENT := {[?A_RULE], []}}},
        robots:parse(?A_VALID_CONTENT, ?A_VALID_CODE)
    ).

can_parse_valid_non_binary_robots_txt() ->
    [
        {doc,
            "Given a valid robots.txt content in non-binary format, when parsing, "
            "then returns all rules."}
    ].

can_parse_valid_non_binary_robots_txt(_Config) ->
    NonBinary = unicode:characters_to_list(?A_VALID_CONTENT),
    ?assertMatch(
        {ok, #{?REVERSED_USER_AGENT := {[?A_RULE], []}}},
        robots:parse(NonBinary, ?A_VALID_CODE)
    ).

can_handle_malformed_content() ->
    [{doc, "Given a malformed content, when parsing, then ignores the malformed part."}].

can_handle_malformed_content(_Config) ->
    ?assertMatch(
        {ok, _},
        robots:parse(?A_MALFORMED_CONTENT, ?A_VALID_CODE)
    ).

can_fetch_sitemap() ->
    [{doc, "Given content with sitemap, when parsing, then returns the sitemap."}].

can_fetch_sitemap(_Config) ->
    {ok, RulesIndex} = robots:parse(?CONTENT_WITH_SITEMAP, ?A_VALID_CODE),

    ?assertMatch({ok, ?SITEMAP}, robots:sitemap(RulesIndex)).

return_error_on_non_existent_sitemap() ->
    [{doc, "Given content without sitemap, when parsing, then returns an error."}].

return_error_on_non_existent_sitemap(_Config) ->
    {ok, RulesIndex} = robots:parse(?A_VALID_CONTENT, ?A_VALID_CODE),

    ?assertMatch({error, not_found}, robots:sitemap(RulesIndex)).

allow_all_on_unmatched_agents_at_end_of_file() ->
    [
        {doc,
            "Given unmatched agents at the end of the file, when parsing, "
            "then allows everything for those agents."}
    ].

allow_all_on_unmatched_agents_at_end_of_file(_Config) ->
    ?assertMatch(
        {ok, #{?REVERSED_USER_AGENT := {allowed, all}}},
        robots:parse(<<"User-Agent: ", ?USER_AGENT/binary>>, ?A_VALID_CODE)
    ).

merge_repeated_agent() ->
    [
        {doc,
            "Given a rules index with the same user agent repeated, when parsing, then merges the rules."}
    ].
merge_repeated_agent(_Config) ->
    ?assertMatch(
        {ok, #{?REVERSED_USER_AGENT := {[<<"/foo/*">>], [<<"/bar">>]}}},
        robots:parse(?SOME_CONTENT_WITH_REPEATED_AGENTS, ?A_VALID_CODE)
    ).

ignore_inline_comments() ->
    [{doc, "Given a rule with a comment in it, when parsing, then ignores the comment."}].

ignore_inline_comments(_Config) ->
    ?assertMatch(
        {ok, #{?REVERSED_USER_AGENT := {[?A_RULE], []}}},
        robots:parse(?A_VALID_CONTENT_WITH_COMMENT, ?A_VALID_CODE)
    ).

return_true_if_agent_is_allowed() ->
    [
        {doc,
            "Given a rules index with allowed URL for the corresponding agent, "
            "when checking if allowed, then returns true."}
    ].

return_true_if_agent_is_allowed(_Config) ->
    {ok, RulesIndex} = robots:parse(?ANOTHER_VALID_CONTENT, ?A_VALID_CODE),

    ?assert(robots:is_allowed(?USER_AGENT, ?A_MATCHING_URL, RulesIndex)).

match_independently_of_the_casing_of_the_agent() ->
    [
        {doc,
            "Given a rules index with allowed URL for the corresponding agent, "
            "when checking if allowed with the allowed agent in different casing, "
            "then returns true."}
    ].
match_independently_of_the_casing_of_the_agent(_Config) ->
    {ok, RulesIndex} = robots:parse(?ANOTHER_VALID_CONTENT, ?A_VALID_CODE),

    ?assert(robots:is_allowed(string:uppercase(?USER_AGENT), ?A_MATCHING_URL, RulesIndex)).

return_false_if_agent_is_disallowed() ->
    [
        {doc,
            "Given a rules index with disallowed URL for the corresponding agent, "
            "when checking if allowed, then returns false."}
    ].

return_false_if_agent_is_disallowed(_Config) ->
    {ok, RulesIndex} = robots:parse(?ANOTHER_VALID_CONTENT, ?A_VALID_CODE),

    ?assertNot(robots:is_allowed(?USER_AGENT, ?ANOTHER_MATCHING_URL, RulesIndex)).

return_true_if_no_matching_rules_can_be_found() ->
    [
        {doc,
            "Given a rules index with no matching agent, when checking if allowed, "
            "then returns true."}
    ].

return_true_if_no_matching_rules_can_be_found(_Config) ->
    {ok, RulesIndex} = robots:parse(?ANOTHER_VALID_CONTENT, ?A_VALID_CODE),

    ?assert(robots:is_allowed(?NON_EXISTENT_USER_AGENT, ?ANOTHER_MATCHING_URL, RulesIndex)).

return_true_if_everything_is_allowed_for_the_corresponding_agent() ->
    [
        {doc,
            "Given a rules index with an agent for which everything is allowed, "
            "when checking if allowed, then returns true."}
    ].
return_true_if_everything_is_allowed_for_the_corresponding_agent(_Config) ->
    {ok, RulesIndex} = robots:parse(<<"User-Agent: ", ?USER_AGENT/binary>>, ?A_VALID_CODE),

    ?assert(robots:is_allowed(?USER_AGENT, ?AN_URL, RulesIndex)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
