-module(proxy_sup).
-author('yh@gmail.com').
-behaviour(supervisor).

-include("proxy.hrl").
-export([start_link/0]).

-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%log(Module, Line, _Level, FormatFun) ->
%%	{Format, Arguments} = FormatFun(),
%%	file_log_server:send(debug, self(), Module, Line, Format, Arguments).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.

init([]) ->
	{success, {ListenIp, ListenPort}} = world_config:get_cfg(recharge_callback_listen_addr),
	Http = web_specs(proxy_recharge_web, world_util:ipstr_to_v4(ListenIp), ListenPort),


	ProxyServerMgr =
		{proxy_server_mgr,
			{proxy_server_mgr, start_link, []},
			permanent,
			5000,
			worker,
			[proxy_server_mgr]},
    {ok, 
    	{
    		{one_for_one, 10, 10},
			[ProxyServerMgr, Http]
    	}
    }.

web_specs(Mod, ListenIp, Port) ->
	WebConfig = [{ip, ListenIp},
		{port, Port},
		{nodelay, true},
		{docroot, world_http_deps:local_path(["priv", "www"])}],
	{Mod,
		{Mod, start, [WebConfig]},
		permanent, 5000, worker, dynamic}.


