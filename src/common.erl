-module(common).
-include("usr.hrl").

%% API helpers %%
-export([
         read_body/1,
         get_usr_data_with_body/1,
         get_old_and_new_pass_with_body/1,
         send_list_usrs_to_client/3
        ]).

%% API conditions %%
-export([
         is_auth/1,
         is_valid_pass/1,
         is_valid_login/1,
         is_valid_req_usr/1
        ]).

%% read_body - read body with request %%
%% @Req = request
%% return json object with body
read_body(Req) ->
    read_body(Req, <<>>).
read_body(Req0, Acc) ->
  case cowboy_req:read_body(Req0) of
    {ok, Data, _Req} ->
      {ok, <<Acc/binary, Data/binary>>};
    {more, Data, Req} ->
      read_body(Req, <<Acc/binary, Data/binary>>)
  end.

%% get_usr_data_with_body - parse json object and return usr %%
%% @Req = request
%% return Usr ident(login and password)
get_usr_data_with_body(Req) ->
  {ok, Json} = read_body(Req),
  {Attr} = jiffy:decode(Json),
  try
    #usr{login=binary_to_list(proplists:get_value(?LOGIN, Attr)),
         password=binary_to_list(proplists:get_value(?PASS, Attr))}
   catch
   	_:_ ->
   	  {err,notValReq}
   end.

%% get_old_and_new_pass_with_body - parse json object %%
%% and return old and new passwod tuple               %%
%% @Req = request
%% return old and new password
get_old_and_new_pass_with_body(Req) ->
  {ok, Json} = read_body(Req),
  {Attr} = jiffy:decode(Json),
  try
    {binary_to_list(proplists:get_value(?OLD_PASS, Attr)),
     binary_to_list(proplists:get_value(?NEW_PASS, Attr))}
   catch
   	_:_ ->
   	  {err,notValReq}
   end.

%% send_list_usrs_to_client - send list users to client %%
%% @List = list all users
%% @Req = request
%% if list empty send fin flags

send_list_usrs_to_client([], Req,State) ->
  cowboy_req:stream_body(<<"ok">>,fin,Req),
  {ok, Req, State};
send_list_usrs_to_client([Head | Tail],Req,State) ->
  cowboy_req:stream_body(list_to_binary(Head), nofin, Req),
  cowboy_req:stream_body(<<"\n">>, nofin, Req),
  send_list_usrs_to_client(Tail,Req,State).

%% is_auth - control auth with usr %%
%% @Req = request
%% return true | false auth
is_auth(Req) ->
  Cookies = cowboy_req:parse_cookies(Req),
  case lists:keyfind(<<"LoginId">>, 1, Cookies) of
    {_, Login} ->
      case ets:lookup(?DB_NAME, binary_to_list(Login)) of
        [] -> {err, noAuth};
        _  -> {ok, Login}
      end;
    _ -> {err, noSessId}
  end.

%% is_valid_req_usr - valid user request? %%
%% @Usr = record #usr{}
%% return true | false(if not valid request)
is_valid_req_usr(Usr) ->
  case is_valid_login(Usr#usr.login) of
    true ->
      case is_valid_pass(Usr#usr.password) of
        true -> valid;
          _ -> notValidPass
      end;
     _ -> notValidLogin
  end.

%% is_valid_pass %%
%% @Password = password user
%% return true | false
is_valid_pass(Password)
  when length(Password) >= 8, length(Password) < 16 ->
    is_valid_str(Password);
is_valid_pass(_) ->
  false.

%% is_valid_login %%
%% @Password = login user
%% return true | false
is_valid_login(Login)
  when length(Login) >= 2, length(Login) < 255 ->
    is_valid_str(Login);
is_valid_login(_) ->
  false.

%% is_valid_str %%
%% @List = string
%% return true | false
is_valid_str([Char | Rest])
  when Char >= $a, Char =< $z;
       Char >= $A, Char =< $Z;
       Char >= $0, Char =< $9 ->
    is_valid_str(Rest);
is_valid_str([]) ->
  true;
is_valid_str(_) ->
  false.
