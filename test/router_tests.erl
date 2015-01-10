

-module(router_tests).

-include_lib("eunit/include/eunit.hrl").

-include_lib("router/include/router.hrl").

new_test() ->
    Router = router:new(),
    true = router:delete(Router),
    ok.

multiple_new_test() ->
    Router1 = router:new(),
    Router2 = router:new(),
    Router3 = router:new(),

    true = router:delete(Router1),
    true = router:delete(Router2),
    true = router:delete(Router3),

    ok.

add_test() ->
    Router = router:new(),

    _ = router:add(Router, [<<"a">>, <<"b">>], a),
    _ = router:add(Router, [<<"a">>, <<"c">>], a),

    router:delete(Router).

remove_test() ->
    Router = router:new(),

    _ = router:add(Router, [<<"a">>, <<"b">>], a),
    _ = router:add(Router, [<<"a">>, <<"b">>], b),

    _ = router:add(Router, [<<"a">>, <<"c">>], a),
    _ = router:add(Router, [<<"a">>, <<"c">>], b),

    ?assertEqual(2, length(router:get_paths(Router, a))),
    ?assertEqual(2, length(router:get_paths(Router, b))),

    router:remove(Router, a),
    ?assertEqual(0, length(router:get_paths(Router, a))),
    ?assertEqual(2, length(router:get_paths(Router, b))),

    router:remove(Router, b),
    ?assertEqual(0, length(router:get_paths(Router, a))),
    ?assertEqual(0, length(router:get_paths(Router, b))),

    ok.

get_paths_test() ->
    Router = router:new(),

    _ = router:add(Router, [<<"a">>, <<"b">>], {1, a}),
    _ = router:add(Router, [<<"a">>, <<"c">>], {2, a}),
    _ = router:add(Router, [<<"a">>, <<"d">>], {3, a}),

    R = router:get_paths(Router, {1, '_'}),
    ?assertEqual([{[<<"a">>, <<"b">>], {1, a}}], R),

    _ = router:add(Router, [<<"a">>, <<"b">>], {1, b}),

    R1 = router:get_paths(Router, {1, '_'}),
    ?assertEqual([{[<<"a">>, <<"b">>], {1, a}},
            {[<<"a">>, <<"b">>], {1, b}}], lists:sort(R1)),

    _ = router:add(Router, [<<"foo">>, <<"bar">>, <<"baz">>], {1, c}),

    R2 = router:get_paths(Router, {1, '_'}),
    ?assertEqual([{[<<"a">>, <<"b">>], {1, a}},
            {[<<"a">>, <<"b">>], {1, b}},
            {[<<"foo">>, <<"bar">>, <<"baz">>], {1, c}} ], lists:sort(R2)),

    ok.

match_test() ->
    Router = router:new(),
    Result = router:match(Router, [<<"a">>, <<"b">>]),
    ?assertEqual([], Result),

    %% Two paths
    _ = router:add(Router, [<<"a">>, <<"b">>], {1, a}),
    _ = router:add(Router, [<<"a">>, <<"b">>], {1, b}),

    %% And a simple wildcard path
    _ = router:add(Router, [<<"a">>, '+'], {1, c}),

    R1 = router:match(Router, [<<"a">>, <<"b">>]),
    ?assertEqual([
            [<<"a">>, '+'],
            [<<"a">>, <<"b">>]
        ], lists:sort(R1)),

    ok.

match_wildcard_test() ->
    Router = router:new(),

    _ = router:add(Router, [<<"a">>, {'+', id}], a),
    R1 = router:match(Router, [<<"a">>, <<"b">>]),
    ?assertEqual([[<<"a">>, {'+', id}]], R1),

    ok.

route_wildcard_test() ->
    Router = router:new(),

    _ = router:add(Router, [<<"a">>, {'+', id}], a),
    _ = router:add(Router, ['#'], b),

    R1 = router:route(Router, [<<"a">>, <<"b">>]),
    ?assertEqual([#route{destination=a, bound_args=[{id, <<"b">>}]},
                  #route{destination=b, bound_args=[{'#', [<<"a">>, <<"b">>]}]} ], lists:sort(R1)),

    ok.

match_mf_wildcard_test() ->
    Router = router:new(),
    _ = router:add(Router, [<<"a">>, {'+', id, {erlang, is_integer}}], a),

    R1 = router:match(Router, [<<"a">>, <<"b">>]),
    ?assertEqual([], R1),

    R2 = router:match(Router, [<<"a">>, 100]),
    ?assertEqual([[<<"a">>, {'+', id, {erlang, is_integer}}]], R2),

    ok.
