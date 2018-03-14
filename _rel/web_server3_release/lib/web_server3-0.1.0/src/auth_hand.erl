-module(auth_hand).
-behaviour(cowboy_handler).
-include("usr.hrl").

-export([init/2]).

init(Req=#{method := <<"GET">>},State) ->
  requests:usr_auth(Req, State);
init(Req,State) ->
  reply_serv:reply_method_not_allowed(Req,State,?BIN_ERR("notAllowed")).