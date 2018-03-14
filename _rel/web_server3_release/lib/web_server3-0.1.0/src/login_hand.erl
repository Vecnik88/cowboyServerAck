-module(login_hand).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
  requests:user_change_password(Req, State).