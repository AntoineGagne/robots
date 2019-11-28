-module(robots_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(UNSUPPORTED_CODE, 109).
-define(CODE_4XX, 418).
-define(CODE_5XX, 514).
-define(EMPTY_CONTENT, <<>>).

all() ->
    [
     return_error_on_unsupported_status_code,
     allow_all_on_4xx_code,
     disallow_all_on_5xx
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
