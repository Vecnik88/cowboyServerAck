{application, 'web_server3', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['all_usr_hand','auth_hand','common','login_hand','reg_hand','reply_serv','requests','usr_db','web_server3_app','web_server3_sup']},
	{registered, [web_server3_sup]},
	{applications, [kernel,stdlib,cowboy,jiffy]},
	{mod, {web_server3_app, []}},
	{env, []}
]}.