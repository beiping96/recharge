%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 六月 2015 22:31
%%%-------------------------------------------------------------------
-module(proxy_server_mgr).
-author("yaohong").

-behaviour(gen_server).

-include("../deps/file_log/include/file_log.hrl").

%% API
-export([start_link/0]).
-export([get_callback_url/1]).
%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3,
	reset_callback_url/0,
	add_callback_url/3,
	delete_callback_url/1]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
get_callback_url(ServerId) ->
    gen_server:call(?SERVER, {get_callback_url, world_util:to_integer(ServerId)}).

reset_callback_url() ->
	?SERVER ! reset_callback_url.

add_callback_url(ServerId, Ip, Port) ->
	gen_server:cast(?SERVER, {add_callback_url, ServerId, Ip, Port}).

delete_callback_url(ServerId) ->
	gen_server:cast(?SERVER, {delete_callback_url, ServerId}).

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
	ets:new(server_callback, [set, protected, named_table]),

    Path = os:getenv("SERVER_CALLBACK_CFG"),
    {ok, Terms} = file:read_file(Path),
    {ok, {"server", _, ServerList}, _} = erlsom:simple_form(binary_to_list(Terms)),

    ?FILE_LOG_DEBUG("ServerList is ~p", [ServerList]),

    lists:foreach(fun({_, Values, _}) ->
        ServerIP = proplists:get_value("ip", Values),
        ServerPort = proplists:get_value("port", Values),
        ServerId = proplists:get_value("id", Values),

        CallbackAddr = "http://" ++ ServerIP ++ ":" ++ ServerPort,
        ets:insert(server_callback, {world_util:to_integer(ServerId), CallbackAddr})
    end, ServerList),
	{ok, #state{}}.

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
handle_call({get_callback_url, ServerId}, _From, State) ->
    Ret =
        try
            case ets:lookup(server_callback, ServerId) of
                [{_, Callback}]  ->
                    {success, Callback};
                [] ->
                    throw({custom, "get_callback_url failed"})
            end
        catch What:Type->
            ?FILE_LOG_DEBUG("ServerId is ~p, What is ~p, type is ~p, strace is ~p,", [ServerId, What, Type, erlang:get_stacktrace()]),
            fail
        end,
	{reply, Ret, State};
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
handle_cast({add_callback_url, ServerId, ServerIp, ServerPort}, State) ->
	CallbackAddr = "http://" ++ ServerIp ++ ":" ++ integer_to_list(ServerPort),
	ets:insert(server_callback, {world_util:to_integer(ServerId), CallbackAddr}),
	?FILE_LOG_DEBUG("server_callback : ~p", [ets:tab2list(server_callback)]),
	{noreply, State};

handle_cast({delete_callback_url, ServerId}, State) ->
	ets:delete(server_callback, world_util:to_integer(ServerId)),
	?FILE_LOG_DEBUG("server_callback : ~p", [ets:tab2list(server_callback)]),
	{noreply, State};

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
handle_info(reset_callback_url, State) ->
	try
		Path = os:getenv("SERVER_CALLBACK_CFG"),
		{ok, Terms} = file:read_file(Path),
		{ok, {"server", _, ServerList}, _} = erlsom:simple_form(binary_to_list(Terms)),

		?FILE_LOG_DEBUG("Reset ServerList is ~p", [ServerList]),

		lists:foreach(fun({_, Values, _}) ->
			ServerIP = proplists:get_value("ip", Values),
			ServerPort = proplists:get_value("port", Values),
			ServerId = proplists:get_value("id", Values),

			CallbackAddr = "http://" ++ ServerIP ++ ":" ++ ServerPort,
			ets:insert(server_callback, {world_util:to_integer(ServerId), CallbackAddr})
		              end, ServerList),
		{noreply, State}
	catch
		What : Reason ->
			?FILE_LOG_ERROR("reset callback url error!!! ~p,~p,~p", [What, Reason, erlang:get_stacktrace()]),
			{noreply, State}
	end;
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
	State :: #state{}) -> term()).
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
