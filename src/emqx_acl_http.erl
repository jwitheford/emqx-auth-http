%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_acl_http).

-behaviour(emqx_acl_mod).

-include("emqx_auth_http.hrl").

-include_lib("emqx/include/emqx.hrl").

-import(emqx_auth_http_cli, [request/3, feedvar/2, feedvar/3]).

%% ACL callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

init(AclReq) ->
	{ok, #{acl_req => AclReq}}.

check_acl({Credentials, PubSub, Topic}, #{acl_req := #http_request{
                                            method = Method,
                                            url = Url,
                                            params = Params}}) ->
    Params1 = feedvar(feedvar(feedvar(Params, Credentials), "%A", access(PubSub)), "%t", Topic),
    {_, Username} = lists:keyfind("username", 1, Params1),
    RealmFromUsername = lists:nth(1, string:split(Username, "_")),
    RealmFromTopic = lists:nth(1, string:split(Topic, "/")),
    if
        Username == <<"jwt">> -> allow;
        true ->
        if
            RealmFromUsername == RealmFromTopic -> 
                logger:debug("Realm:~p from topic matches realm:~p", [RealmFromTopic, RealmFromUsername]),
                allow;
            true -> 
                logger:error("Realm:~p from topic mismatches realm:~p", [RealmFromTopic, RealmFromUsername]),
                deny
        end
    end.

access(subscribe) -> 1;
access(publish)   -> 2.

reload_acl(_State) -> ok.

description() -> "ACL with HTTP API".
