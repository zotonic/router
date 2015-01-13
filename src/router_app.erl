%% @author Maas-Maarten <mmzeeman@xs4all.nl>
%% @copyright 2015 Maas-Maarten Zeeman

%% Copyright 2015 Maas-Maarten Zeeman

%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.


-module(router_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    %% Start our application supervisor
    router_sup:start_link().

stop(_State) ->
    ok.

