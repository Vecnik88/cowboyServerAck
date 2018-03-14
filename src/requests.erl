-module(requests).
-include("usr.hrl").
-export([
         usr_auth/2,
         get_usr_list/2,
         usr_registration/2,
         usr_change_password/2
        ]).

%% user_registration - register user %%
%% @Req = request to server
%% @State = state
%% return reply ok | err
usr_registration(Req,State) ->
    case Usr = common:get_usr_data_with_body(Req) of
      {err,notValReq} ->
        reply_serv:reply_bad_req(Req,State,?BIN_ERR("badRequest"));
      _ ->
        case common:is_valid_req_usr(Usr) of
          valid ->
            case usr_db:usr_already_reg(Usr) of
              {error,errLogin} ->
                usr_db:add_usr(Usr),
                reply_serv:reply_ok_data(Req,State,?BIN_OK("registration"));
              {error,errPass} ->
                reply_serv:reply_false_ident(Req,State,?BIN_ERR("userAlreadyReg"));
              {ok,registration} ->
                reply_serv:reply_false_ident(Req,State,?BIN_ERR("userAlreadyReg"))
            end;
          notValidPass ->
            reply_serv:reply_false_ident(Req,State,?BIN_ERR("notValidPass"));
          notValidLogin ->
            reply_serv:reply_false_ident(Req,State,?BIN_ERR("notValidLogin"))
        end
    end.

%% user_auth - user authentication %%
%% @Req = request to server
%% @State = state
%% return reply ok | err
usr_auth(Req,State) ->
  Auth = common:is_auth(Req),
  if Auth ->
    reply_serv:reply_ok_data(Req,State,?BIN_OK("alreadyAuth"));
  true ->
    case Usr = common:get_usr_data_with_body(Req) of
      {err,notValReq} ->
        reply_serv:reply_bad_req(Req,State,?BIN_ERR("badRequest"));
      _ ->
        case common:is_valid_req_usr(Usr) of
          valid ->
            case usr_db:usr_already_reg(Usr) of
              {error,errLogin} ->
                reply_serv:reply_false_ident(Req,State,?BIN_ERR("notFindUser"));
              {error,errPass} ->
                reply_serv:reply_false_ident(Req,State,?BIN_ERR("notCorrectPass"));
              {ok,registration} ->
                ReqReply = cowboy_req:set_resp_cookie(<<"LoginId">>,Usr#usr.login,Req,
                                                      #{http_only => true, max_age => 3600}),
                reply_serv:reply_ok_data(ReqReply,State,?BIN_OK("auth"))
            end;
          notValidPass ->
            reply_serv:reply_false_ident(Req,State,?BIN_ERR("notValidPass"));
          notValidLogin ->
            reply_serv:reply_false_ident(Req,State,?BIN_ERR("notValidLogin"))
        end
      end
  end.

%% get_user_list - request to list with all user %%
%% @Req = request to server
%% @State = state
%% return reply bin list | forbidden operation
get_usr_list(Req,State) ->
  Auth = common:is_auth(Req),
    if Auth ->
      List = list_to_binary(ets:match(?DB_NAME, #usr{login='$0', password='_'})),
        reply_serv:reply_ok(Req,State,List);
    true ->
        reply_serv:reply_forbidden_req(Req,State,?BIN_ERR("notAuthReq"))
  end.

usr_change_password(Req,State) ->
  Auth = common:is_auth(Req),
  if Auth ->
    Login = cowboy_req:binding(login,Req),
    ParsedQs = cowboy_req:parse_qs(Req),
    case ParsedQs of
      [{?OLD_PASS, OldPassBin}, {?NEW_PASS,NewPassBin}] ->
      Usr = #usr{login=binary_to_list(Login),
                 password=binary_to_list(OldPassBin)},
      case usr_db:is_valid_req_usr(Usr) of
        valid ->
          NewPassList = common:is_valid_pass(binary_to_list(NewPassBin)),
          if NewPassList ->
            case usr_db:usr_already_reg(Usr) of
              {error,errLogin} ->
                reply_serv:reply_false_ident(Req,State,?BIN_OK("notFindUser"));
              {error,errPass} ->
                reply_serv:reply_false_ident(Req,State,?BIN_ERR("errOldPass"));
              {ok,registration} ->
              Cookies = cowboy_req:parse_cookies(Req),
              {_,LoginAuth} = lists:keyfind(<<"LoginId">>,1,Cookies),
                if Login =:= LoginAuth ->
                  usr_db:update_usr(Usr#usr{password=NewPassList}),
                  reply_serv:reply_ok_data(Req,State,?BIN_ERR("updatePass"));
                true ->
                  reply_serv:reply_forbidden_req(Req,State,?BIN_ERR("forbiddenOperation"))
                end
            end;
          true ->
            reply_serv:reply_false_ident(Req,State,?BIN_ERR("notValidNewPass"))
          end;
        _ ->
          reply_serv:reply_bad_req(Req,State,?BIN_ERR("notValReq"))
      end;
      _ ->
        reply_serv:reply_bad_req(Req,State,?BIN_ERR("notValReq"))
    end;
  true ->
   reply_serv:reply_forbidden_req(Req,State,?BIN_ERR("notAuthReq"))
  end.
