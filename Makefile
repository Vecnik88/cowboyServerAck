PROJECT = web_server3
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

DEPS = cowboy jiffy
dep_cowboy_commit = 2.2.2
dep_jiffy = git https://github.com/davisp/jiffy master
DEP_PLUGINS = cowboy

include erlang.mk
