-module(login_hand).
-behaviour(cowboy_handler).
-include("usr.hrl").

-export([init/2]).

init(Req=#{method := <<"PUT">>},State) ->
  requests:usr_change_password(Req, State);
init(Req,State) ->
  reply_serv:reply_method_not_allowed(Req,State,?BIN_ERR("notAllowed")).
