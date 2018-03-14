-module(all_usr_hand).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req,State) ->
  requests:get_usr_list(Req,State).