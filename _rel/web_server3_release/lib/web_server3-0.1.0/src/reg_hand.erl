-module(reg_hand).
-include("usr.hrl").
-behavior(cowboy_handler).

-export([init/2]).

init(Req=#{method := <<"POST">>},State) ->
  requests:usr_registration(Req,State);
init(Req,State) ->
  reply_serv:reply_method_not_allowed(Req,State,?BIN_ERR("notAllowed")).
