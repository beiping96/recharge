%% @author Mochi Media <dev@mochimedia.com>

%% @copyright 2010 Mochi Media <dev@mochimedia.com>



%% @doc Web server for payment.



-module(proxy_recharge_web).

-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).
-export([mod_check/1]).

%% External API

-include("../deps/file_log/include/file_log.hrl").

start(Options) ->
	{DocRoot, Options1} = get_option(docroot, Options),
	Loop =
		fun(Req) ->
			?MODULE:loop(Req, DocRoot)
		end,
	mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).
stop() ->
	mochiweb_http:stop(?MODULE).

loop(Req, _DocRoot) ->
	try
		"/" ++ ModName = Req:get(path),
		%%去除后缀
		AtomModName = ?MODULE:mod_check(ModName),

		RetValue = AtomModName:req_handle(Req),
		Req:respond({200, [], RetValue})
	catch
		throw:{custom, Reason} ->
			?FILE_LOG_DEBUG("bh_recharge callback fail reason=~p", [Reason]),
			Req:respond({500, [], world_util:to_list(Reason)});
		Type:What ->
			?FILE_LOG_ERROR("exception type=~p, what=~p, stack=~p", [Type, What, erlang:get_stacktrace()]),
			Req:respond({500, [], "system error"})
	end.


mod_check("proxy_v2_recharge") -> mod_proxy_v2_recharge;
mod_check("gaeapay") -> mod_proxy_gaea_recharge;
mod_check(Other) -> throw({custom, Other}).

%% Internal API

get_option(Option, Options) ->

	{proplists:get_value(Option, Options), proplists:delete(Option, Options)}.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


you_should_write_a_test() ->
	?assertEqual(
		"No, but I will!",
		"Have you written any tests?"),
	ok.
-endif.


