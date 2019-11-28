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
-define(AN_URL, <<"/bot-url">>).
-define(A_RULE, <<"/foo/*">>).
-define(A_VALID_CODE, 200).
-define(A_VALID_CONTENT, <<"User-Agent: ", ?USER_AGENT/binary, "\nAllow: ", ?A_RULE/binary>>).

all() ->
    [
     return_error_on_unsupported_status_code,
     allow_all_on_4xx_code,
     disallow_all_on_5xx,
     return_true_if_everything_is_allowed,
     return_false_if_everything_is_disallowed,
     can_parse_valid_robots_txt
    ].

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    Config.

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
    [{doc, "Given a set of rules that specifies that everything is allowed, "
      "when checking if allowed, then returns true."}].
return_true_if_everything_is_allowed(_Config) ->
    ?assert(robots:is_allowed(?USER_AGENT, ?AN_URL, {allowed, all})).

return_false_if_everything_is_disallowed() ->
    [{doc, "Given a set of rules that specifies that everything is allowed, "
      "when checking if allowed, then returns false."}].
return_false_if_everything_is_disallowed(_Config) ->
    ?assertNot(robots:is_allowed(?USER_AGENT, ?AN_URL, {disallowed, all})).

can_parse_valid_robots_txt() ->
    [{doc, "Given a valid robots.txt content, when parsing, then returns all rules."}].
can_parse_valid_robots_txt(_Config) ->
    ?assertMatch({ok, #{?USER_AGENT := {[?A_RULE], []}}},
                 robots:parse(?A_VALID_CONTENT, ?A_VALID_CODE)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
