-module(reply_serv).
-include("usr.hrl").

%% macro reply code %%
-define(OK,200).
-define(OK_DATA,201).
-define(BAD_REQ,400).
-define(BAD_AUTH_PARAM,401).
-define(FORBID_REQ,403).
-define(NOT_ALLOWED,405).
-define(SERV_ERR,500).

-export([
        reply_ok/3,
        reply_bad_req/3,
        reply_serv_err/3,
        reply_ok_data/3,
        reply_false_ident/3,
        reply_forbidden_req/3,
        reply_method_not_allowed/3
        ]).

%% 200 - ОК %%
reply_ok(Req,State,BinData) ->
  Req0 = cowboy_req:reply(?OK,
                          ?CONT_TYPE_TXT_PLAIN,
                          BinData,
                          Req),
  {ok, Req0, State}.

%% 201 Request ok %%
reply_ok_data(Req,State,BinData) ->
  Req0 = cowboy_req:reply(?OK_DATA,
                          ?CONT_TYPE_TXT_PLAIN,
                          BinData,
                          Req),
  {ok, Req0, State}.

%% 400 - Bad request %%
reply_bad_req(Req,State,BinData) ->
  Req0 = cowboy_req:reply(?BAD_REQ,
                          ?CONT_TYPE_TXT_PLAIN,
                          BinData,
                          Req),
  {ok, Req0, State}.

%% 401 is not auth %%
reply_false_ident(Req,State,BinData) ->
  Req0 = cowboy_req:reply(?BAD_AUTH_PARAM,
                          ?CONT_TYPE_TXT_PLAIN,
                          BinData,
                          Req),
  {ok, Req0, State}.

%% 403 forbidden request %%
reply_forbidden_req(Req,State,BinData) ->
  Req0 = cowboy_req:reply(?FORBID_REQ,
                          ?CONT_TYPE_TXT_PLAIN,
                          BinData,
                          Req),
  {ok, Req0, State}.

%% 405 method not allowed %%
reply_method_not_allowed(Req, State, BinData) ->
  Req0 = cowboy_req:reply(?NOT_ALLOWED,
                          ?CONT_TYPE_TXT_PLAIN,
                          BinData,
                          Req),
  {ok, Req0, State}.

%% 500 - Internal Server Error %%
reply_serv_err(Req,State,BinData) ->
  Req0 = cowboy_req:reply(?SERV_ERR,
                          ?CONT_TYPE_TXT_PLAIN,
                          BinData,
                          Req),
  {ok, Req0, State}.
