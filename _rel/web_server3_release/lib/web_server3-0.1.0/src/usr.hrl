%% usr.hrl %%
-record(usr, {login, password}).

-define(ANY_HOST,'_').
-define(DB_NAME,timeWeb).
-define(LOGIN,<<"login">>).
-define(PASS,<<"password">>).
-define(OLD_PASS,<<"old_password">>).
-define(NEW_PASS,<<"new_password">>).
-define(CONT_TYPE_TXT_PLAIN,
        #{<<"content-type">> => <<"text/plain">>}).

-define(BIN_OK(Msg),<<"{ok, " Msg "}">>).
-define(BIN_ERR(Msg),<<"{err, " Msg "}">>).
