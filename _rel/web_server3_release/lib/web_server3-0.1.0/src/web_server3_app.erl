-module(web_server3_app).
-include("usr.hrl").
-behaviour(application).

-export([start/2,stop/1]).

start(_Type,_Args) ->
  usr_db:create_table(),
  Dispatch = cowboy_router:compile([
    {?ANY_HOST, [ {"/user/registration",reg_hand,#{}},
                  {"/user/auth",auth_hand,#{}},
                  {"/user/:login",login_hand,#{}},
                  {"/user",all_usr_hand,#{}}
    ]}
  ]),
  cowboy:start_clear(http_test_work,
                     [{port, 8080}],
                     #{env => #{dispatch => Dispatch}}
                    ),
  web_server3_sup:start_link().

stop(_State) ->
  usr_db:close_table(),
  ok.
