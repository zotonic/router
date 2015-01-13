%% @author Maas-Maarten <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman

%% @doc: router application supervisor
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

-module(router_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0
]).

%% Supervisor callbacks
-export([init/1]).

%%
%% Api
%%

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { 
       {one_for_all, 5, 10}, 
       [{router_reg, {router_reg, start_link, []}, 
         permanent, 5000, worker, [router_reg]}]
      }}.

