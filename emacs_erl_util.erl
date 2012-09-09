
-module(emacs_erl_util).

-export([start/0, stop/0]).

start() ->
	inc_test_compiler:start_link(),
	emacs_shell_reloader:start().

stop() ->
	inc_test_compiler:stop(),
	emacs_shell_reloader:stop().

