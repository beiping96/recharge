-module(proxy_app).
-author('yh@gmail.com').
-behaviour(application).
-include("../deps/file_log/include/file_log.hrl").
-include("proxy.hrl").

-export([start/2,
    stop/1,
    prep_stop/1]).

-spec start(Type :: term(), StartArgs :: term()) ->
    {ok, pid()} | {error, 'badarg'}.
start(normal, _StartArgs) ->
    world_ctl:write_pid_file(),
    ok = world_util:ensure_app_started(log4erl),
    ok = world_util:ensure_app_started(sasl),
    ok = world_util:ensure_app_started(crypto),
    ok = world_util:ensure_app_started(asn1),
    ok = world_util:ensure_app_started(public_key),
    ok = world_util:ensure_app_started(ssl),
    ok = world_util:ensure_app_started(inets),
%% 	wait_mnesia_table(),
    {ok, _} = world_config:start_link(),
    {ok, Pid} = proxy_sup:start_link(),
    log_running_applications(),
    ?FILE_LOG_INFO("proxy ~s is started in the node ~p", [?PROXY_VERSION, node()]),
    {ok, Pid};
start(_Type, _StartArgs) ->
    {error, 'badarg'}.

prep_stop(State) ->
    State.

%% wait_mnesia_table() ->
%% 	case mnesia:wait_for_tables([global_config], 3 * 1000) of
%% 		ok -> ok;
%% 		{timeout, BadTabeList} ->
%% 			?FILE_LOG_ERROR("wait_for_tables [~p] timeout bad_table_list=~p", [global_config, BadTabeList]),
%% 			exit("wait_for_tables global_config timeout");
%% 		{error, Reason} ->
%% 			?FILE_LOG_ERROR("wait_for_tables [~p] timeout reason=~p", [global_config, Reason]),
%% 			exit("wait_for_tables global_config error")
%% 	end.


stop(_State) ->
    world_ctl:delete_pid_file(),
    ?FILE_LOG_INFO("proxy ~s is stopped in the node ~p", [?PROXY_VERSION, node()]),
    ok.

-spec log_running_applications() ->
    ok.
log_running_applications() ->
    lists:foreach(
        fun(App) ->
            ?FILE_LOG_INFO("running application#~p", [App])
        end, application:which_applications()).

