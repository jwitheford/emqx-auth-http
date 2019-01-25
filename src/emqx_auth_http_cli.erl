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

-module(emqx_auth_http_cli).

-include_lib("emqx/include/emqx.hrl").

-export([request/3, feedvar/2, feedvar/3, request2IDrest/3]).

%%--------------------------------------------------------------------
%% HTTP Request
%%--------------------------------------------------------------------

request2IDrest(get, Url, Params) ->
    %%  token is passed as password in MQTT client
    {_, Token} = lists:keyfind("password", 1, Params),
    %% realmID_puffID is passed as username in MQTT client
    {_, Username} = lists:keyfind("username", 1, Params),
    Realm = binary_to_list(lists:nth(1, string:split(Username, "_"))),

    Auth = "Bearer " ++ binary_to_list(Token),
    LoginUrl = string:join(string:replace(Url, "{}", Realm), ""),
    Req = {LoginUrl, [{"Authorization", Auth}, {"User-Agent", "albi_internal"}]},
    logger:debug("Reaching out to id-rest to verify token: ~p", [Auth]),
    reply(request_(get, Req, [{autoredirect, true}], [], 0)).

request(get, Url, Params) ->
    Req = {Url ++ "?" ++ mochiweb_util:urlencode(Params), []},
    reply(request_(get, Req, [{autoredirect, true}], [], 0));

request(post, Url, Params) ->
    Req = {Url, [], "application/x-www-form-urlencoded", cow_qs:qs(bin_kw(Params))},
    reply(request_(post, Req, [{autoredirect, true}], [], 0)).

request_(Method, Req, HTTPOpts, Opts, Times) ->
    %% Resend request, when TCP closed by remotely
    case httpc:request(Method, Req, HTTPOpts, Opts) of
        {error, socket_closed_remotely} when Times < 3 ->
            timer:sleep(trunc(math:pow(10, Times))),
            request_(Method, Req, HTTPOpts, Opts, Times+1);
        Other -> Other
    end.

reply({ok, {{_, Code, _}, _Headers, Body}}) ->
    logger:debug("Receiving ok response code:~p, headers:~p, body:~p", [Code, _Headers, Body]),
    {ok, Code, Body};
reply({ok, Code, Body}) ->
    logger:debug("Receiving ok response code:~p, body:~p", [Code, Body]),
    {ok, Code, Body};
reply({error, Error}) ->
    logger:debug("Receiving error response error:~p", [Error]),
    {error, Error}.

%% TODO: move this conversion to cuttlefish config and schema
bin_kw(KeywordList) when is_list(KeywordList) ->
    [{bin(K), bin(V)} || {K, V} <- KeywordList].

bin(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom));
bin(Int) when is_integer(Int) ->
    integer_to_binary(Int);
bin(Float) when is_float(Float) ->
    float_to_binary(Float, [{decimals, 12}, compact]);
bin(List) when is_list(List)->
    list_to_binary(List);
bin(Binary) when is_binary(Binary) ->
    Binary.

%%--------------------------------------------------------------------
%% Feed Variables
%%--------------------------------------------------------------------

feedvar(Params, _Credentials = #{username := Username, client_id := ClientId, peername := {IpAddr, _}}) ->
    lists:map(fun({Param, "%u"}) -> {Param, Username};
                 ({Param, "%c"}) -> {Param, ClientId};
                 ({Param, "%a"}) -> {Param, inet:ntoa(IpAddr)};
                 ({Param, Var})  -> {Param, Var}
              end, Params).

feedvar(Params, Var, Val) ->
    lists:map(fun({Param, Var0}) when Var0 == Var ->
                      {Param, Val};
                 ({Param, Var0}) ->
                      {Param, Var0}
              end, Params).
