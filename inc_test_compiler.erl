%%%-------------------------------------------------------------------
%%% @author 姚 海涛 <>
%%% @copyright (C) 2012, 姚 海涛
%%% @doc
%%%
%%% @end
%%% Created :  8 Sep 2012 by 姚 海涛 <>
%%%-------------------------------------------------------------------
-module(inc_test_compiler).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
%% -define(DEBUG(String, Args), io:format("[DEBUG] " ++ String, Args)).
-define(DEBUG(String, Args), ok).
-define(INFO(String, Args), io:format("[INFO] " ++ String, Args)).
-record(state, {stamp, tref, src_dir, ebin_dir, file_name_re}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() ->
    gen_server:cast({local, ?SERVER}, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, Pwd} = file:get_cwd(),
    {ok, TRef} = timer:send_interval(timer:seconds(2), compile),
    {ok, FileNameRe} = re:compile(".erl$"),
    {ok, #state{stamp = stamp(), tref = TRef,
	        src_dir = Pwd ++ "/src",
		ebin_dir = Pwd ++ "/ebin",
		file_name_re = FileNameRe
		}}.
stamp() ->
    calendar:local_time().
%% @private
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private

handle_info(compile, State = #state{stamp = Stamp, src_dir = SrcDir, ebin_dir = EbinDir, file_name_re = FileNameRe}) ->
    case file:list_dir(SrcDir) of
	{error,enoent} ->
	    {noreply, State};
	{ok, FileNames} ->
	    ModifiedFiles = filter_modified_src(SrcDir, FileNames, Stamp, FileNameRe),
	    compile_files(ModifiedFiles, EbinDir),
	    {noreply, State#state{stamp = stamp()}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

compile_files([], _EbinDir) ->
    ok;
compile_files([Src|Rest], EbinDir) ->
    Ret = compile:file(Src, [{d, 'TEST'}, debug_info, {outdir, EbinDir}]),
    ?INFO("compile log: ~p~n", [Ret]),
    compile_files(Rest, EbinDir).

filter_modified_src(SrcDir, FileNames, Stamp, FileNameRe) ->
    Acc = do_filter(SrcDir, FileNames, calendar:datetime_to_gregorian_seconds(Stamp), [], FileNameRe),
    Acc.
do_filter(_SrcDir, [], _Stamp, Acc, _FileNameRe) ->
    Acc;
do_filter(SrcDir, [F|Rest], Stamp, Acc, FileNameRe) ->
    case re:run(F, FileNameRe) of
	nomatch ->
	    ?DEBUG("invalid file: ~s~n", [F]),
	    do_filter(SrcDir, Rest, Stamp, Acc, FileNameRe);
        {match, _} ->
	    FStamp = filelib:last_modified(SrcDir ++ "/" ++ F),
	    ?DEBUG("file: ~s, time: ~p~n", [F, FStamp]),
	    Fseconds = case FStamp of
			   0 ->
			       0;
			   _ ->
			       calendar:datetime_to_gregorian_seconds(FStamp)
		       end,
	    case Fseconds > Stamp of
		true ->
		    ?INFO("new file to compile, ~s~n", [F]),
		    do_filter(SrcDir, Rest, Stamp, [SrcDir ++ "/" ++ F|Acc], FileNameRe);
		false ->
		    do_filter(SrcDir, Rest, Stamp, Acc, FileNameRe)
	    end
    end.
%% @private
terminate(_Reason, State) ->
    timer:cancel(State#state.tref),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
