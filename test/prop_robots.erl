-module(prop_robots).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(EMPTY_CONTENT, <<>>).

prop_allow_4xx() ->
    ?FORALL(Code, '4xx'(),
            begin
                equals({ok, {allowed, all}}, robots:parse(?EMPTY_CONTENT, Code))
            end).

prop_disallow_5xx() ->
    ?FORALL(Code, '5xx'(),
            begin
                equals({ok, {disallowed, all}}, robots:parse(?EMPTY_CONTENT, Code))
            end).

prop_error_on_unsupported_codes() ->
    ?FORALL(Code, unsupported(),
            begin
                equals({error, {invalid_status_code, Code}}, robots:parse(?EMPTY_CONTENT, Code))
            end).

%%%===================================================================
%%% Generators
%%%===================================================================

'5xx'() ->
    range(500, 599).

'4xx'() ->
    range(400, 499).

unsupported() ->
    oneof([range(100, 199), range(300, 399)]).
