%% usr_db.erl  %%
-module(usr_db).
-include("usr.hrl").

%% API database %%
-export([
         create_table/0,
         close_table/0
         ]).

%% API user with database %%
-export([
         add_usr/1,
         update_usr/1,
         usr_already_reg/1
        ]).

%% create_table - create ets table %%
create_table() ->
  ets:new(?DB_NAME, [named_table, public, {keypos, #usr.login}]).

%% close_table - delete ets table %%
close_table() ->
  ets:delete(?DB_NAME).

%% add_usr - cadd user to database %%
add_usr(Usr) ->
  ets:insert(?DB_NAME, Usr).

%% update - update user with database %%
update_usr(Usr) ->
  add_usr(Usr).

%% usr_already_reg %%
%% @Usr = record #usr{}
%% return ok | err
usr_already_reg(Usr) ->
  case ets:lookup(?DB_NAME, Usr#usr.login) of
    [] -> {error, errLogin};
      _  ->
        case ets:match(?DB_NAME, Usr) of
          [] -> {error, errPass};
          _  -> {ok, registration}
         end
  end.

