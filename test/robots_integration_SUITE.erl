-module(robots_integration_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(VALID_ROBOTS, "valid-robots.txt").
-define(A_VALID_CODE, 200).

all() ->
    [
        can_parse_valid_robots_txt
    ].

init_per_suite(Config) ->
    Dir = ?config(data_dir, Config),
    {ok, Valid} = file:read_file(filename:join(Dir, ?VALID_ROBOTS)),
    [{valid, Valid} | Config].

end_per_suite(Config) ->
    Config.

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    Config.

can_parse_valid_robots_txt() ->
    [{doc, "Given a valid robots.txt, when parsing, then returns valid rules index."}].

can_parse_valid_robots_txt(Config) ->
    Valid = ?config(valid, Config),

    ?assertMatch(
        {ok, #{<<"tobrettiwt">> := {[<<"/imgres">>], []}}},
        robots:parse(Valid, ?A_VALID_CODE)
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
