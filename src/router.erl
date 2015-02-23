%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2014-2015 Maas-Maarten Zeeman
%%
%% @doc In-memory trie router for fast parallel path lookups with wildcards.
%% Copyright 2014-2015 Maas-Maarten Zeeman
%%

%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.

%% This code based on the mnesia based mqtt topic router by 
%% <ery.lee@gmail.com> found in emqtt.

-module(router).

-author("Maas-Maarten Zeeman <mmzeeman@xs4all.nl>").

-export([new/0, new/1, delete/1]).
-export([info/1]).

-export([add/3, remove/2, remove_path/3]).

-export([paths/1]).
-export([get_paths/2]).

-export([match/2, match/3]).
-export([route/2, route/3]).

-include_lib("router.hrl").

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-record(router, {
    node_table :: ets:tid(), %% record(trie_node) 
    trie_table :: ets:tid() , %% record(trie) 
    wildcard_table :: ets:tid(), %% record(wildcard).
    path_table :: ets:tid(), % record(path) Table with paths
    destination_table :: ets:tid() % record(destination), table with destinations
}).


-opaque router() :: atom() | #router{}.
-type route() :: #route{}.

-type single_level_wildcard() :: '+' | {'+', atom()} | {'+', atom(), binary()} | {'+', atom(), {atom(), atom()}}.
-type multi_level_wildcard() :: '#'.
-type wildcard() :: single_level_wildcard() | multi_level_wildcard().
-type path_entry() :: binary() | integer() | wildcard().
-type path() :: list(path_entry()).

-type destination() :: any().

-export_type([
    router/0, 
    path/0, 
    destination/0, 
    path_entry/0, 
    wildcard/0, 
    single_level_wildcard/0, 
    multi_level_wildcard/0, 
    route/0
]).

%%
%% Internal Records
%%

-record(path, {
    name :: path()
}).

-record(trie_edge, {
    node_id :: path(),
    word :: path_entry()
}).

-record(wildcard, {
    node_id :: path(),     
    wildcard :: single_level_wildcard()
}).

-record(trie, {
    edge :: record(trie_edge),
    node_id :: path()
}).

-record(trie_node, {
    node_id :: path(),
    edge_count = 0 :: pos_integer(),

    path :: path()
}). 

-record(destination, {
    path :: path(),
    destination :: destination()
}).

%%
%% Api
%%

%% @doc Create a new router. Creates ets table owned by the calling process.
%%
-spec new() -> router().
new() ->
    NodeTable = ets:new(node_table, [protected, set, {read_concurrency, true}, {keypos, 2}]),
    TrieTable = ets:new(trie_table, [protected, set, {read_concurrency, true}, {keypos, 2}]),

    WildcardTable = ets:new(wildcard_table, [protected, bag, {read_concurrency, true}, {keypos, 2}]),
    PathTable = ets:new(path_table, [protected, bag, {read_concurrency, true}, {keypos, 2}]),
    DestinationTable = ets:new(destination_table, [protected, bag, {read_concurrency, true}, {keypos, 2}]),

    #router{node_table=NodeTable, 
        trie_table=TrieTable, 
        wildcard_table=WildcardTable,
        path_table=PathTable, 
        destination_table=DestinationTable}.

% @doc
-spec new(atom()) -> router().
new(Name) ->
    Router = new(),
    ok = router_reg:register(Name, Router),
    Router.

%% @doc Get usage statistics 
%%
info(#router{}=Router) ->
    [{nodes, ets:info(Router#router.node_table, size)}, 
     {edges, ets:info(Router#router.trie_table, size)}, 
     {wildcards, ets:info(Router#router.wildcard_table, size)}, 
     {paths, ets:info(Router#router.path_table, size)}, 
     {destinations, ets:info(Router#router.destination_table, size)}];
info(Name) ->
    info(router_reg:router(Name)).

%% @doc Delete the router. Deletes the all the ets tables.
%%
-spec delete(router()) -> true.
delete(Name) when is_atom(Name) ->
    Router = router_reg:router(Name),
    router_reg:unregister(Name),
    delete(Router);
delete(#router{node_table=NodeTable, wildcard_table=WildcardTable, trie_table=TrieTable, 
               path_table=PathTable, destination_table=DestinationTable}) ->
    true = ets:delete(NodeTable),
    true = ets:delete(TrieTable),
    true = ets:delete(WildcardTable),
    true = ets:delete(PathTable),
    true = ets:delete(DestinationTable).

% @doc Add a path to the router. Important: Make sure add is called synchronized.
%
-spec add(router(), path(), destination()) -> ok.
add(#router{}=Router, Path, Destination) ->
    trie_add(Router, Path),
    ets:insert(Router#router.destination_table, #destination{path=Path, destination=Destination}),
    ok;
add(Name, Path, Destination) ->
    add(router_reg:router(Name), Path, Destination).

% @doc Remove a destination. All paths to the destinations are removed. Important:
% make sure remove is called synchronized.
%
-spec remove(router(), destination()) -> ok.
remove(#router{}=Router, Destination) ->
    Paths = ets:match_object(Router#router.destination_table, #destination{destination=Destination, _ = '_'}), 
    [begin 
         ets:delete_object(Router#router.destination_table, Dest),
         try_remove_path(Router, Path)
     end || #destination{path=Path}=Dest <- Paths],
    ok;
remove(Name, Destination) ->
    remove(router_reg:router(Name), Destination).

% @doc Remove path
remove_path(#router{}=Router, Path, Destination) ->
    ets:match_delete(Router#router.destination_table, #destination{path=Path, destination=Destination, _='_'}),
    try_remove_path(Router, Path);
remove_path(Name, Path, Destination) ->
    remove_path(router_reg:router(Name), Path, Destination).


% @doc Get all paths registered in the router.
-spec paths(router()) -> list(path()).
paths(#router{}=Router) ->
    [Path || #path{name=Path} <- ets:tab2list(Router#router.path_table)];
paths(Name) ->
    paths(router_reg:router(Name)).


%% @doc Get the associated paths from a match spec. 
%%
-spec get_paths(router(), term()) -> list({path(), destination()}).
get_paths(#router{destination_table=DestTable}, MatchSpec) ->
    Objects = ets:match_object(DestTable, #destination{path='_', destination=MatchSpec}),
    [{Path, Dest} || #destination{path=Path, destination=Dest} <- Objects];
get_paths(Name, MatchSpec) ->
    get_paths(router_reg:router(Name), MatchSpec).


%% @doc Return matching paths. 
%%
-spec match(router(), path()) -> list(path()).
match(Router, Path) ->
    match(Router, Path, []).

-spec match(router(), path(), term()) -> list(path()).
match(#router{}=Router, Path, Args) ->
    TrieNodes = trie_match(Router, root, Path, Args),
    Paths = [
        begin
                case ets:lookup(Router#router.path_table, NodePath) of
                    [] -> undefined;
                    [#path{name=Name}] -> Name
                end 
        end || #trie_node{path=NodePath} <- TrieNodes, NodePath =/= undefined],
    [E || E <- Paths, E =/= undefined];
match(Name, Path, Args) ->
    match(router_reg:router(Name), Path, Args).


%% @doc Return matching destinations. and
%%
-spec route(router(), path()) -> list(route()).
route(Router, Path) ->
    route(Router, Path, []).

-spec route(router(), path(), term()) -> list(route()).
route(#router{}=Router, Path, Args) ->
    Matches = match(Router, Path, Args),
    route1(Router, Path, Matches, []);
route(Name, Path, Args) ->
    route(router_reg:router(Name), Path, Args).

route1(_Router, _Path, [], Acc) ->
    lists:flatten(Acc);
route1(Router, Path, [Match|Matches], Acc) ->
    Bind = bind(Path, Match),
    Destinations = [#route{destination=D, bound_args=Bind} || #destination{destination=D} <-  
                        ets:lookup(Router#router.destination_table, Match)],
    route1(Router, Path, Matches, [Destinations | Acc]).


%% Bind variables from the match to the path
%%
bind(Path, Match) ->
    bind(Path, Match, []).

bind([], [], Acc) ->
    lists:reverse(Acc);
bind([_H|_Path]=P, ['#'|_Rest], Acc) ->
    lists:reverse([{'#', P}|Acc]);
bind([H|Path], ['+'|Match], Acc) ->
    bind(Path, Match, [H|Acc]);
bind([H|Path], [{'+', Atom}|Match], Acc) ->
    bind(Path, Match, [{Atom, H}|Acc]);
bind([H|Path], [{'+', Atom, _M}|Match], Acc) ->
    bind(Path, Match, [{Atom, H}|Acc]);
bind([_|Path], [_|Match], Acc) ->
    bind(Path, Match, Acc).



%%
%% Helpers
%%

trie_match(Router, NodeId, [], ResAcc) ->
    Found = ets:lookup(Router#router.node_table, NodeId),
    Found ++ 'trie_match_#'(Router, NodeId, ResAcc);

trie_match(Router, NodeId, [W | Words], ResAcc) ->
    MatchingWildcards = [Wc || #wildcard{wildcard=Wc} <- ets:lookup(Router#router.wildcard_table, NodeId), 
                               matches_wildcard(W, Wc, [])], 
    lists:foldl(
      fun(WArg, Acc) -> 
              case ets:lookup(Router#router.trie_table, #trie_edge{node_id=NodeId, word=WArg}) of
                  [#trie{node_id=ChildId}] ->
                      trie_match(Router, ChildId, Words, Acc);
                  [] ->
                      Acc
              end
      end, 
      'trie_match_#'(Router, NodeId, ResAcc), [W | MatchingWildcards]). 


%%
'trie_match_#'(Router, NodeId, ResAcc) ->
    case ets:lookup(Router#router.trie_table, #trie_edge{node_id=NodeId, word='#'}) of
        [#trie{node_id=ChildId}] ->
            Found = ets:lookup(Router#router.node_table, ChildId),
            Found ++ ResAcc;
        [] ->
            ResAcc
    end.

% Single level wildcard match.
% TODO: add regex matcher.
matches_wildcard(_Word, '+', _Args) -> 
    true;
matches_wildcard(_Word, {'+', Atom}, _Args) when is_atom(Atom) -> 
    true;
matches_wildcard(Word, {'+', Atom, {M, F}}, Args) when is_atom(Atom) -> 
    case erlang:function_exported(M, F, 2) of
        true -> M:F(Word, Args);
        false -> M:F(Word)
    end;

matches_wildcard(_Word, _, _) -> 
    false.

trie_add(Router, Path) ->
    %% This should be called synchronization.
    do_trie_add(Router, Path).

trie_add_path(Router, Triple) ->
    %% This should be called synchronzied.
    do_trie_add_path(Router, Triple).

do_trie_add_path(Router, {Node, Word, Child}) ->
    Edge = #trie_edge{node_id=Node, word=Word},

    case ets:lookup(Router#router.node_table, Node) of
        [#trie_node{edge_count=_Count}] ->
            case ets:lookup(Router#router.trie_table, Edge) of
                [] ->
                    Trie = #trie{edge=Edge, node_id=Child},
                    ets:update_counter(Router#router.node_table, Node, {#trie_node.edge_count, +1}),
                    ets:insert(Router#router.trie_table, Trie),
                    insert_wildcard(Router, Node, Word);
                [_] -> 
                    ok
            end;
        [] ->
            TrieNode = #trie_node{node_id=Node, edge_count=1},
            Trie = #trie{edge=Edge, node_id=Child},
            ets:insert(Router#router.node_table, TrieNode),
            ets:insert(Router#router.trie_table, Trie),
            insert_wildcard(Router, Node, Word)
    end.

%% Remove Path
%%
try_remove_path(Router, Path) ->
    %% This should be called synchronized
    case ets:member(Router#router.destination_table, Path) of
        false ->
            PathRecord = #path{name=Path},
            ets:delete_object(Router#router.path_table, PathRecord),
            case ets:lookup(Router#router.path_table, Path) of
                [] ->
                    trie_delete(Router, Path);
                _ ->
                    ignore
            end;
        true -> 
            ok
        end.

trie_delete(Router, Path) ->
    case ets:lookup(Router#router.node_table, Path) of
        [#trie_node{edge_count=0}] ->
            ets:delete(Router#router.node_table, Path),
            ets:delete(Router#router.wildcard_table, Path),
            trie_delete_path(Router, lists:reverse(triples(Path)));
        [#trie_node{path=NodePath}] when NodePath =/= Path->
            ets:update_element(Router#router.node_table, Path, {#trie_node.path, Path});
        _ ->
            ignore
    end.

trie_delete_path(_Router, []) ->
    ok;
trie_delete_path(Router, [{NodeId, Word, _} | RestPath]) ->
    Edge = #trie_edge{node_id=NodeId, word=Word},
    ets:delete(Router#router.trie_table, Edge),

    case ets:lookup(Router#router.node_table, NodeId) of
        [#trie_node{edge_count=1, path=undefined}] ->
            ets:delete(Router#router.node_table, NodeId),
            ets:delete(Router#router.wildcard_table, NodeId),
            trie_delete_path(Router, RestPath);
        [#trie_node{edge_count=1, path=Path}] ->
            ets:update_counter(Router#router.node_table, NodeId, {#trie_node.edge_count, -1}),
            case ets:lookup(Router#router.path_table, Path) of
                [] ->
                    %% This topic is gone too.
                    trie_delete(Router, Path);
                _ ->
                    ok
            end;
        [#trie_node{edge_count=Count}] when Count >= 1 ->
            ets:update_counter(Router#router.node_table, NodeId, {#trie_node.edge_count, -1});

        [] ->
            throw({notfound, NodeId})
    end.

%%
%%
insert_wildcard(Router, NodeId, '+') ->
    do_insert_wildcard(Router, #wildcard{node_id=NodeId, wildcard='+'});
insert_wildcard(Router, NodeId, {'+', _Id}=Wildcard) ->
    do_insert_wildcard(Router, #wildcard{node_id=NodeId, wildcard=Wildcard});
insert_wildcard(Router, NodeId, {'+', _Id, _Match}=Wildcard) ->
    do_insert_wildcard(Router, #wildcard{node_id=NodeId, wildcard=Wildcard});
insert_wildcard(_Router, _NodeId, _Word) ->
    ok.

do_insert_wildcard(Router, Wildcard) ->
    ets:insert(Router#router.wildcard_table, Wildcard).

do_trie_add(Router, Path) ->
    ets:insert(Router#router.path_table, path(Path)),

    %% Lookup a node with node id Path
    case ets:lookup(Router#router.node_table, Path) of
        [#trie_node{path=Path}] ->
            ok;
        [#trie_node{path=undefined}] ->
            ets:update_element(Router#router.node_table, Path, {#trie_node.path, Path});
        [] ->
            Triples = triples(Path),
            [trie_add_path(Router, Triple) || Triple <- Triples],
            ets:insert(Router#router.node_table, #trie_node{node_id=Path, path=Path})
    end.


triples(Path) ->
    triples(Path, [], []).

%% triples(term()) -> {Node, Word, Child}
triples([], [], []) ->
    [{root, [], []}];
triples([], _Path, Acc) ->
    lists:reverse(Acc);

triples([A|Rest], [], []) ->
    triples(Rest, [A], [{root, A, [A]}]);
triples([A|Rest], Path, Acc) ->
    NewPath = Path ++ [A],
    triples(Rest, NewPath, [{Path, A, NewPath}|Acc]).

-spec path(path()) -> record(path).
path(Path) ->
    #path{name=Path}.

%%
%% Unit-Tests
%%

-ifdef(TEST).

add_wildcard_routes_test() ->
    Router = new(),
    _ = router:add(Router, [<<"a">>, <<"b">>], a),

    W1 = ets:match(Router#router.wildcard_table, '$1'),
    ?assertEqual([], W1),

    _ = router:add(Router, [<<"a">>, <<"b">>, '+'], a),

    W2 = ets:match(Router#router.wildcard_table, '$1'),
    ?assertEqual([#wildcard{node_id=[<<"a">>, <<"b">>], wildcard='+'}], lists:flatten(W2)),

    _ = router:add(Router, [<<"a">>, {'+', test}], a),

    W3 = ets:match(Router#router.wildcard_table, '$1'),
    ?assertEqual([#wildcard{node_id=[<<"a">>], wildcard={'+', test}},
                  #wildcard{node_id=[<<"a">>, <<"b">>], wildcard='+'}], lists:sort(lists:flatten(W3))),

    _ = router:add(Router, [<<"a">>, {'+', test, <<"regex">>}], a),

    W4 = ets:match(Router#router.wildcard_table, '$1'),
    ?assertEqual([#wildcard{node_id=[<<"a">>], wildcard={'+', test}},
                  #wildcard{node_id=[<<"a">>], wildcard={'+', test, <<"regex">>}},
                  #wildcard{node_id=[<<"a">>, <<"b">>], wildcard='+'}], lists:sort(lists:flatten(W4))),

    _ = router:add(Router, ['+', <<"b">>], b),
    W5 = ets:match(Router#router.wildcard_table, '$1'),
    %% Note, this doesn't add a wildcard...
    ?assertEqual([#wildcard{node_id=root, wildcard='+'},
                  #wildcard{node_id=[<<"a">>], wildcard={'+', test}},
                  #wildcard{node_id=[<<"a">>], wildcard={'+', test, <<"regex">>}},
                  #wildcard{node_id=[<<"a">>, <<"b">>], wildcard='+'}], lists:sort(lists:flatten(W5))),

    _ = router:add(Router, ['#'], c),
    W6 = ets:match(Router#router.wildcard_table, '$1'),
    ?assertEqual([#wildcard{node_id=root, wildcard='+'},
                  #wildcard{node_id=[<<"a">>], wildcard={'+', test}},
                  #wildcard{node_id=[<<"a">>], wildcard={'+', test, <<"regex">>}},
                  #wildcard{node_id=[<<"a">>, <<"b">>], wildcard='+'}], lists:sort(lists:flatten(W6))),

    _ = router:add(Router, [<<"a">>, '#'], c),
    W7 = ets:match(Router#router.wildcard_table, '$1'),
    ?assertEqual([#wildcard{node_id=root, wildcard='+'},
                  #wildcard{node_id=[<<"a">>], wildcard={'+', test}},
                  #wildcard{node_id=[<<"a">>], wildcard={'+', test, <<"regex">>}},
                  #wildcard{node_id=[<<"a">>, <<"b">>], wildcard='+'}], lists:sort(lists:flatten(W7))),

    ok.

triples_test() ->
    T = triples([<<"a">>, <<"b">>, <<"c">>, <<"d">>]),
    ?assertEqual([{root,<<"a">>,[<<"a">>]},
            {[<<"a">>],<<"b">>,[<<"a">>,<<"b">>]},
            {[<<"a">>,<<"b">>],<<"c">>,[<<"a">>,<<"b">>,<<"c">>]},
            {[<<"a">>,<<"b">>,<<"c">>],<<"d">>,[<<"a">>,<<"b">>,<<"c">>,<<"d">>]}], T),

    ok.

triples_props() ->
    ?FORALL(
        Path,
        list(weighted_union([
                    {1, single_level_wildcard()}, 
                    {5, binary()}
                ])),
        begin
                Triples = triples(Path),

                case Path of 
                    [] ->
                        ?assertEqual(1, length(Triples)),
                        true;
                    _ -> 
                        ?assertEqual(length(Triples), length(Path)),
                        true
                end
        end).

triples_props_test() ->
    ?assertEqual(true, proper:quickcheck(triples_props(), [{to_file, user}])),
    ok.


-endif.
