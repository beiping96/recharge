%%%-------------------------------------------------------------------
%%% @author mayday
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 六月 2017 14:40
%%%-------------------------------------------------------------------
-module(time_impl).
-author("mayday").
-include("../deps/yhsql/include/yhsql.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {inc_second}).
-export([
	inc_second/1
        ]).
-export([sync_inc_hour/2, sync_inc_hour/1]).
-export([sync_inc_minute/1, sync_inc_minute/2]).
-export([sync_inc_second/2]).
-export([all_node/0]).
-export([load_inc_second/0]).


-export([
	get_current_time/0,
	timestamp/0,
	local_time/0,
	universal_time/0
        ]).
%%%===================================================================
%%% API
%%%===================================================================


inc_second(S) when is_integer(S) andalso S > 0 ->
	gen_server:call(?MODULE, {inc_second, S}).


sync_inc_hour(H) when is_integer(H) andalso H > 0 ->
	sync_inc_hour(H, true).
sync_inc_hour(H, IsSyncDb) when is_boolean(IsSyncDb) ->
	sync_inc_second(H * 60 * 60, IsSyncDb).

sync_inc_minute(M) when is_integer(M) andalso M > 0 ->
	sync_inc_minute(M, true).
sync_inc_minute(M, IsSyncDb) when is_boolean(IsSyncDb) ->
	sync_inc_second(M * 60, IsSyncDb).

sync_inc_second(S, IsSyncDb) when is_integer(S) andalso S > 0 ->
	%%增加的秒数
	if
		IsSyncDb =:= true ->
			success = inc_second_to_db(S);
		true -> ok
	end,
	Nodes = lists:usort(all_node()),
	{success, Mode} = world_config:get_cfg(mode),
	{success, SceneNode} = world_config:get_cfg(scene_node),
	if
		Mode =:= debug ->
%%			rpc:call(SceneNode, time_impl, inc_second, [S]),
      lists:map(
        fun(Node) ->
          Reply = rpc:call(Node, time_impl, inc_second, [S]),
          {Node, Reply}
        end, Nodes),
      rpc:call(SceneNode, scene_manager, set_inc_time, [S]),
			ok;
		true ->
			ok
	end,
	ok.

all_node() ->
	{success, AuctionNode} = world_config:get_cfg(auction_node),
	{success, MailNode} = world_config:get_cfg(mail_node),
	{success, GuildNode} = world_config:get_cfg(guild_node),
	{success, WorldNode} = world_config:get_cfg(world_node),
	{success, IdNode} = world_config:get_cfg(id_node),
	{success, FriendNode} = world_config:get_cfg(friend_node),
	{success, GatewayNode} = world_config:get_cfg(gateway_node),
	{success, OssNode} = world_config:get_cfg(oss_node),
	{success, RankNode} = world_config:get_cfg(rank_node),
	{success, PvpNode} = world_config:get_cfg(pvp_node),
	{success, RcserviceNode} = world_config:get_cfg(rcservice_node),
	{success, HomeBattleNode} = world_config:get_cfg(home_battle_node),
	{success, SceneNode} = world_config:get_cfg(scene_node),
	{success, ActorMgrNode} = world_config:get_cfg(actor_mgr_node),
	{success, RawActorNodeList} = world_config:get_cfg(actor_node_cluster),
	ActorNodeList = [ActorNode || {_, ActorNode} <- RawActorNodeList],
	[
		AuctionNode,
		MailNode,
		GuildNode,
		WorldNode,
		IdNode,
		FriendNode,
		GatewayNode,
		OssNode,
		RankNode,
		PvpNode,
		RcserviceNode,
		HomeBattleNode,
		SceneNode,
		ActorMgrNode | ActorNodeList].



%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
	{ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
	{ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term()} | ignore).
init([]) ->
	{success, IncSecond} = load_inc_second(),
	{ok, #state{inc_second = IncSecond}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
	                 {reply, Reply :: term(), NewState :: #state{}} |
	                 {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
	                 {noreply, NewState :: #state{}} |
	                 {noreply, NewState :: #state{}, timeout() | hibernate} |
	                 {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
	                 {stop, Reason :: term(), NewState :: #state{}}).
handle_call(timestamp, _From, #state{inc_second = IncSecond} = State) ->
	{MSecs, Secs, MicroSecs} = os:timestamp(),
	{reply, {success, {MSecs, Secs + IncSecond, MicroSecs}}, State};
handle_call({inc_second, S}, _From, #state{inc_second = IncSecond} = State) ->
	{reply, success, State#state{inc_second = IncSecond + S}};
handle_call(_Request, _From, State) ->
	{reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
	{noreply, NewState :: #state{}} |
	{noreply, NewState :: #state{}, timeout() | hibernate} |
	{stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) ->
	               term()).
terminate(_Reason, _State) ->
	ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
	                 {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_inc_second() ->
	TableName = "zb_global_config",
	SelectSql = yhsql_util:select_query(TableName, ["gvalue"], "`gkey`='inc_second'"),
	{data, #yhsql_result{rows = [[StrIncSecond]]}} = yhsql:fetch(sql:pool_name(), SelectSql),
	{success, world_util:to_integer(StrIncSecond)}.

inc_second_to_db(IncSecond) ->
	{success, OldIncSecond} = load_inc_second(),
	NewIncSecond = OldIncSecond + IncSecond,
	TableName = "zb_global_config",
	UpdateSql = yhsql_util:update_query(TableName, ["gvalue"], [world_util:to_binary(NewIncSecond)], "`gkey`='inc_second'"),
	{updated, #yhsql_result{affectedrows = _}} = yhsql:fetch(sql:pool_name(), UpdateSql),
	success.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


timestamp() ->
	case world_config:get_cfg(mode) of
		{success, debug} ->
			{success, Time} = gen_server:call(?MODULE, timestamp),
			Time;
		{success, release} ->
			os:timestamp()
	end.

local_time() ->
	case world_config:get_cfg(mode) of
		{success, debug} ->
			Timestamp = time_impl:timestamp(),
			calendar:now_to_local_time(Timestamp);
		{success, release} ->
			calendar:local_time()
	end.

universal_time() ->
	case world_config:get_cfg(mode) of
		{success, debug} ->
			Timestamp = time_impl:timestamp(),
			calendar:now_to_universal_time(Timestamp);
		{success, release} ->
			calendar:universal_time()
	end.

get_current_time() ->
	{{Y, M, D},{H, MI, S}} = time_impl:local_time(),
	L =
		[
			integer_to_list(Y), "-",
			integer_to_list(M), "-",
			integer_to_list(D), "_",
			integer_to_list(H), ":",
			integer_to_list(MI), ":",
			integer_to_list(S)],
	{success, lists:flatten(L)}.