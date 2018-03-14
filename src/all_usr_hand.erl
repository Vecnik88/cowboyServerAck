-module(all_usr_hand).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req=#{method := <<"GET">>},State) ->
  requests:get_usr_list(Req,State);
init(Req,State) ->
  reply_serv:reply_method_not_allowed(Req,State,?BIN_ERR("notAllowed")).