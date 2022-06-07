%% @private
%% @author Maas-Maarten Zeeman <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman
%%
%% @doc Simple registry for routers.
%% @end 
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
%%


-module(router_reg).

-behaviour(gen_server).

-export([
    start_link/0,
    register/2,
    unregister/1,
    update/2,
    router/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(registration, {
    name :: atom() | '_',
    router :: router:router() | '_',
    monitor_ref :: reference() | '_',
    owner_pid :: pid()
}).

%%
%% Api
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% @doc Register router under a name.
%
-spec register(atom(), router:router()) -> ok | {error, already_registered}.
register(Name, Router) ->
    gen_server:call(?MODULE, {register, Name, Router}).

% @doc Unregisters a router.
%
-spec unregister(atom()) -> ok | {error, not_owner}.
unregister(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

% @doc Replace the registration of a router. 
-spec update(atom(), router:router()) -> ok | {error, already_registered}.
update(Name, Router) ->
    gen_server:call(?MODULE, {update, Name, Router}).

% @doc Return the router registered under Name.
%
-spec router(atom()) -> router:router().
router(Name) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            throw(not_found);
        [#registration{router=Router}] ->
            Router
    end.

%%
%% Gen-server callbacks
%%

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?MODULE, [protected, named_table, {read_concurrency, true}, {keypos, 2}]),
    {ok, {}}.

handle_call({register, Name, Router}, {From, _Tag}, State) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            MonitorRef = erlang:monitor(process, From),
            ets:insert(?MODULE, #registration{name=Name, router=Router,
                    monitor_ref=MonitorRef, owner_pid=From}),
            {reply, ok, State};
        [#registration{}] ->
            {reply, {error, already_registered}, State}
    end;

handle_call({unregister, Name}, {From, _Tag}, State) ->
    case ets:lookup(?MODULE, Name) of
        [] ->
            {reply, ok, State};
        [#registration{owner_pid=From, monitor_ref=Ref}=Reg] ->
            erlang:demonitor(Ref),
            ets:delete_object(?MODULE, Reg),
            {reply, ok, State};
        _ ->
            {reply, {error, not_owner}, State}
    end;

handle_call({update, Name, Router}, {From, _Tag}, State) ->
    case ets:lookup(?MODULE, Name) of
        [#registration{owner_pid=From}=Registration] ->
            ets:insert(?MODULE, Registration#registration{router=Router}),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State};
        [#registration{}] ->
            {reply, {error, not_owner}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    {stop, {badreq,Req}, State}.

handle_cast(Msg, State) ->
    {stop, {badmsg, Msg}, State}.

handle_info({'DOWN', _MRef, _Type, Pid, _Info}, State) ->
    %% Remove all subscriptions owned by this Pid.
    Registrations = ets:match_object(?MODULE, #registration{owner_pid=Pid, _='_'}),
    lists:foreach(fun(Reg) -> 
                          erlang:demonitor(Reg#registration.monitor_ref),
                          ets:delete_object(?MODULE, Reg) 
                  end, Registrations),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

