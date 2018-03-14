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
      reply_serv:reply_bad_req(Req,
                               State,
                               ?BIN_ERR("badRequest"));
    _ ->
      case common:is_valid_req_usr(Usr) of
        valid ->
          case usr_db:usr_already_reg(Usr) of
            {error,errLogin} ->
              usr_db:add_usr(Usr),
              reply_serv:reply_ok_data(Req,
                                       State,
                                       ?BIN_OK("registration"));
            {error,errPass} ->
              reply_serv:reply_false_ident(Req,
                                           State,
                                           ?BIN_ERR("userAlreadyReg"));
            {ok,registration} ->
              reply_serv:reply_false_ident(Req,
                                           State,
                                           ?BIN_ERR("userAlreadyReg"))
          end;
        notValidPass ->
          reply_serv:reply_false_ident(Req,
                                       State,
                                       ?BIN_ERR("notValidPass"));
        notValidLogin ->
          reply_serv:reply_false_ident(Req,
                                       State,
                                       ?BIN_ERR("notValidLogin"))
      end
  end.

%% user_auth - user authentication %%
%% @Req = request to server
%% @State = state
%% return reply ok | err
usr_auth(Req,State) ->
  case common:is_auth(Req) of
  {ok,_} ->
    reply_serv:reply_ok_data(Req,
                             State,
                             ?BIN_OK("alreadyAuth"));
  {err, _} ->
    case Usr = common:get_usr_data_with_body(Req) of
      {err,notValReq} ->
        reply_serv:reply_bad_req(Req,
                                 State,
                                 ?BIN_ERR("badRequest"));
      _ ->
        case common:is_valid_req_usr(Usr) of
          valid ->
            case usr_db:usr_already_reg(Usr) of
              {error,errLogin} ->
                reply_serv:reply_false_ident(Req,
                                             State,
                                             ?BIN_ERR("notFindUser"));
              {error,errPass} ->
                reply_serv:reply_false_ident(Req,
                                             State,
                                             ?BIN_ERR("notCorrectPass"));
              {ok,registration} ->
                ReqReply = cowboy_req:set_resp_cookie(<<"LoginId">>,Usr#usr.login,Req,
                                                      #{http_only => true, max_age => 3600}),
                reply_serv:reply_ok_data(ReqReply,
                                         State,
                                         ?BIN_OK("auth"))
            end;
          notValidPass ->
            reply_serv:reply_false_ident(Req,
                                         State,
                                         ?BIN_ERR("notValidPass"));
          notValidLogin ->
            reply_serv:reply_false_ident(Req,
                                         State,
                                         ?BIN_ERR("notValidLogin"))
        end
      end
  end.

%% get_user_list - request to list with all user %%
%% @Req = request to server
%% @State = state
%% return reply bin list | forbidden operation
get_usr_list(Req,State) ->
  case common:is_auth(Req) of
    {ok,_} ->
      List = ets:match(?DB_NAME, #usr{login='$0',password='_'}),
      Req0 = cowboy_req:stream_reply(200,
                                    #{<<"content-type">> => <<"text/plain">>},
                                    Req),
        common:send_list_usrs_to_client(List,Req0,State);
    {err,_} ->
        reply_serv:reply_forbidden_req(Req,
                                       State,
                                       ?BIN_ERR("notAuthReq"))
  end.

%% usr_change_password - change user password %%
%% @Req = request to server
%% @State = state
%% return updatePass | noUpdate
usr_change_password(Req,State) ->
  case common:is_auth(Req) of
    {ok,LoginSess} ->
      Login = cowboy_req:binding(login,Req),
      if Login =:= LoginSess ->
        case common:get_old_and_new_pass_with_body(Req) of
          {OldPass, NewPass} ->
          Usr = #usr{login=binary_to_list(Login),
                     password=OldPass},
          case common:is_valid_req_usr(Usr) of
            valid ->
              NewPassList = common:is_valid_pass(NewPass),
              if NewPassList ->
                case usr_db:usr_already_reg(Usr) of
                  {error,errLogin} ->
                    reply_serv:reply_false_ident(Req,
                                                 State,
                                                 ?BIN_OK("notFindUser"));
                  {error,errPass} ->
                    reply_serv:reply_false_ident(Req,
                                                 State,
                                                 ?BIN_ERR("errOldPass"));
                  {ok,registration} ->
                      usr_db:update_usr(Usr#usr{password=NewPassList}),
                      reply_serv:reply_ok_data(Req,
                                               State,
                                               ?BIN_OK("updatePass"))
                end;
              true ->
                reply_serv:reply_false_ident(Req,
                                             State,
                                             ?BIN_ERR("notValidNewPass"))
              end;
            _ ->
              reply_serv:reply_bad_req(Req,
                                       State,
                                       ?BIN_ERR("notValReq"))
          end;
        _ ->
          reply_serv:reply_bad_req(Req,
                                   State,
                                   ?BIN_ERR("notValReq"))
        end;
      true ->
        reply_serv:reply_forbidden_req(Req,
                                       State,
                                       ?BIN_ERR("forbiddenOperation"))
      end;
    {err,_} ->
      reply_serv:reply_forbidden_req(Req,
                                     State,
                                     ?BIN_ERR("notAuthReq"))
    end.
