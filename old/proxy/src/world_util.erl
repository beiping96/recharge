-module(world_util).
-author('yh@gmail.com').
-export([
	random_value/0,
	ensure_app_started/1,
	ipv4_to_str/1,ipstr_to_v4/1,
	to_integer/1,
	to_binary/1,
	to_float/1,
	to_list/1,
	to_atom/1,
	timestamp/0,
	milliseconds/0,microsecond/0,
	random_in_range/1,
	random_in_range/2,
	max/2,
	min/2,
	bool_to_integer/1,
	integer_to_bool/1,
	is_same_day/2,
	second_to_now/1,
	is_yesterday/2,
	md5_string/1,
	random_list/1,
	time_format/0,time_format/1,
	format/2,
	performance_analyze/3,
	run_time/2,
	reload_mod/1,
	diff_time/2,
	timestamp_3/1,
	timestamp_to_datetime/1,
	get_day/1,
	get_curday_point_timestamp/1,
	get_curday_point_timestamp/2,

	get_server_time_zone/0,
	get_which_day_in_week/0,
	eprof/3,
	eprof/4,
	fprof/4,
	process_infos/1,
	hash/1,
	wait_stop/1,
	create_dir/1,
	get_first_ip/0,
	get_all_ip/0,
	get_all_ip/1,
	garbage_collect/0,

	read_uuid/0,
	is_in_same_day / 3,
	is_in_same_week/2,
	is_in_same_month/2,
	convert_str_time_to_timestamp/1,
	next_time_interval/1,
	param_false_check/2,
	param_undefined_check/2,
	generateWorldTokenId/0,
	send_mail/2,send_mail/3,
	get_list_random_elem/2,

	mac_time_format/0,
	undefined_to_empty/1,

	get_ip/0,
	get_name_length/1,
	check_timestamp/3, check_timestamp_week/3,

	proc_mem_all/1, proc_mem_all/2
]).

-export([to_big/1, to_small/1]).

-export([
        ]).

-export([eksort/1, hex_to_small/1, hex_to_big/1]).

-include("../deps/file_log/include/file_log.hrl").

undefined_to_empty(undefined) -> [];
undefined_to_empty(L) -> L.

-spec random_value() -> integer().
random_value() ->
	trunc((random:uniform() * 1000000)).


-spec ensure_app_started(App :: atom()) -> ok.
ensure_app_started(App) ->
	case application:start(App) of
		ok ->
			ok;
		{error, {already_started, App}} ->
			ok
	end.

-spec ipv4_to_str(Address :: inet:ip4_address()) ->
	string().
ipv4_to_str({A, B, C, D}) ->
	integer_to_list(A) ++ "." ++
	                      integer_to_list(B) ++ "." ++
	                      integer_to_list(C) ++ "." ++
	                      integer_to_list(D);
ipv4_to_str(StrIp4) when is_list(StrIp4) ->
	StrIp4;
ipv4_to_str(_) ->
	"noip".

ipstr_to_v4(IpStr) when is_list(IpStr) ->
	[V1, V2, V3, V4] = string:tokens(IpStr, "."),
	{to_integer(V1), to_integer(V2), to_integer(V3), to_integer(V4)}.

to_integer(I) when is_list(I) ->
	list_to_integer(I);
to_integer(I) when is_binary(I) ->
	to_integer(binary_to_list(I));
to_integer(I) when is_integer(I) ->
	I.

to_float(F) when is_list(F) ->
	TF =
		case lists:member($., F) of
			true -> F;
			false -> F ++ ".0"
		end,
	list_to_float(TF);
to_float(F) when is_integer(F) ->
	to_float(to_list(F) ++ ".0");
to_float(F) when is_float(F) -> F;
to_float(F) when is_binary(F) ->
	list_to_float(binary_to_list(F)).


to_binary(A) when is_binary(A) ->
	A;
to_binary(A) when is_list(A) ->
	iolist_to_binary(A);
to_binary(A) when is_integer(A) ->
	list_to_binary(integer_to_list(A));
to_binary(A) when is_float(A) ->
	to_binary(to_list(A));
to_binary(A) when is_atom(A) ->
	atom_to_binary(A, utf8).

to_list(A) when is_binary(A) ->
	binary_to_list(A);
to_list(A) when is_integer(A) ->
	integer_to_list(A);
to_list(A) when is_float(A) ->
	float_to_list(A, [{decimals, 3}, compact]);
to_list(A) when is_list(A) -> A.




to_atom(A) when is_binary(A) ->
	to_atom(to_list(A));
to_atom(A) when is_list(A) ->
	erlang:list_to_atom(A).


random_in_range(Max) ->
	trunc((random:uniform() * 1000000000)) rem Max.

random_in_range(Min, Max) ->
	if
		Min =:= Max ->
			Min;
		true ->
			trunc((random:uniform() * 1000000000)) rem (Max - Min + 1) + Min
	end.


max(L, R) when L < R -> R;
max(L, _R) -> L.

min(L, R) when L > R -> R;
min(L, _R) -> L.

bool_to_integer(true) -> 1;
bool_to_integer(false) -> 0.

integer_to_bool(0) -> false;
integer_to_bool(_) -> true.





is_same_day(TS1, TS2) when is_integer(TS1) andalso is_integer(TS2) ->
	{{Y1, M1, D1}, _} = second_to_now(TS1),
	{{Y2, M2, D2}, _}  = second_to_now(TS2),
	if
		Y1 =:= Y2 andalso
		M1 =:= M2 andalso
			D1 =:= D2 ->
			true;
		true -> false
	end.

is_yesterday(TS1, TS2) when is_integer(TS1) andalso is_integer(TS2) ->
	{{Y1, M1, D1}, _} = second_to_now(TS1),
	{{Y2, M2, D2}, _}  = second_to_now(TS2),
	case {Y1 =:= Y2, M1 =:= M2, D2 - D1 =:= 1} of
		{true, true, true} -> true;
		_ -> false
	end.

second_to_now(-1) ->
	{{1970, 1, 1}, {7, 59, 59}};
second_to_now(Second) when is_integer(Second) ->
	Time = {Second div 1000000, Second rem 1000000, 0},
	calendar:now_to_local_time(Time).


md5_string(S) ->
	Md5_bin =  erlang:md5(S),
	Md5_list = binary_to_list(Md5_bin),
	lists:flatten(list_to_hex(Md5_list)).

to_big(L) -> [hex_to_big(H) || H <- L].
to_small(L) -> [hex_to_small(H) || H <- L].

hex_to_big(C) when C >= 97 andalso C =< 122 ->  C - 32;
hex_to_big(C) -> C.

hex_to_small(C) when C >= 65 andalso C =< 90 ->  C + 32;
hex_to_small(C) -> C.



%%
%% Local Functions
%%
list_to_hex(L) ->
	lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
	[hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
	$0+N;
hex(N) when N >= 10, N < 16 ->
	$a + (N-10).

random_list(List) when is_list(List) ->
	NewList =
		lists:map(
			fun(Item) ->
				{Item, trunc(random:uniform() * 100000000000)}
			end, List),
	SF =
		fun({_, LRandomValue}, {_, RRandomValue}) ->
			if
				LRandomValue > RRandomValue -> false;
				true -> true
			end
		end,
	[Item || {Item, _} <- lists:sort(SF, NewList)].


time_format() ->
	{{Y, M, D},{H, MI, S}} = time_impl:local_time(),
	time_format({{Y, M, D},{H, MI, S}}).
time_format({{Y, M, D},{H, MI, S}}) ->
	L =
		[
			integer_to_list(Y), "-",
			integer_to_list(M), "-",
			integer_to_list(D), "_",
			integer_to_list(H), ":",
			integer_to_list(MI), ":",
			integer_to_list(S)],
	lists:flatten(L).


mac_time_format() ->
	{{Y, M, D},{H, MI, S}} = time_impl:local_time(),
	mac_time_format({{Y, M, D},{H, MI, S}}).
mac_time_format({{Y, M, D},{H, MI, _}}) ->
	S = io_lib:format("~.4.0w-~.2.0w-~.2.0w ~.2.0w:~.2.0w", [Y, M, D, H, MI]),
	lists:flatten(S).

eksort(P) ->
	Fun =
		fun({LKey, _}, {RKey, _}) ->
			compare(LKey, RKey)
		end,
	lists:sort(Fun, P).

compare([], []) -> true;
compare([], _) -> true;
compare(_, []) -> false;
compare([L|_], [R|_]) when L > R -> false;
compare([L|_], [R|_]) when L < R -> true;
compare([L|T1], [L|T2]) -> compare(T1, T2).


format(Desc, Params) when is_list(Desc) andalso is_list(Params) ->
	lists:flatten(io_lib:format(Desc, Params)).



performance_analyze(Mod, Fun, Args) ->
	eprof:start(),
	eprof:profile([self()], Mod, Fun, Args),
	eprof:stop_profiling(),
	V = eprof:analyze(),
	eprof:stop(),
	V.

run_time(Fun, Count) ->
	T1 = milliseconds(),
	run_1(Count, Fun),
	milliseconds() - T1.

run_1(0, _) -> ok;
run_1(Count, Fun) ->
	Fun(),
	run_1(Count - 1, Fun).

reload_mod(ModName) when is_atom(ModName) ->
	code:soft_purge(ModName) andalso code:load_file(ModName).




diff_time(LTime, RTime) when LTime >= RTime -> LTime - RTime;
diff_time(LTime, RTime) ->
	RTime + 16#FFFFFFFF - LTime.

%% timestamp_1() ->
%% 	T1 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
%% 	T2 = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
%% 	T1 - T2.
%%
%% timestamp_to_datetime(TimeStamp) ->
%% 	T1 = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
%% 	calendar:gregorian_seconds_to_datetime(TimeStamp + T1).

%%时间相关的操作函数
%%获取当天某个点的时间戳

%% 当前时间戳(秒数)
-spec timestamp() ->
	non_neg_integer().
timestamp() ->
	{MSecs, Secs, _} = time_impl:timestamp(),
	MSecs * 1000000 + Secs.
%%获取毫秒数
milliseconds() ->
	{MegaSecs, Secs, MicroSecs} = time_impl:timestamp(),
	1000000000 * MegaSecs + Secs * 1000 + MicroSecs div 1000.

%%获取微秒
microsecond() ->
	{MegaSecs, Secs, USecs} = time_impl:timestamp(),
	1000000000000 * MegaSecs + Secs * 1000000 + USecs.

timestamp_3(Data) ->
	T1 = calendar:datetime_to_gregorian_seconds(Data),
	T2 = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
	T1 - T2.

%获取的是UTC时间（年月日）
timestamp_to_datetime(TimeStamp) ->
	T1 = calendar:datetime_to_gregorian_seconds({{1970,1,1}, {0,0,0}}),
	calendar:gregorian_seconds_to_datetime(TimeStamp + T1).


get_day(Timestamp) -> Timestamp div 86400.

get_curday_point_timestamp(Time) ->
	get_curday_point_timestamp(world_util:timestamp(), Time).
get_curday_point_timestamp(Timestamp, {H, Min, S}) ->
	%%当前天数某个时间点的时间戳
	DayCount = get_day(Timestamp),
	DayCount * 86400  + H * 60 * 60 + Min * 60 + S.
%%获取的是UTC时间的0点（对应的北京时间为上午八点) ,如果国内客户端要显示为凌晨三点刷新，则服务器需要配置的刷新时间为下午7点


%% 计算服务器所在时区(这里特殊处理，因为都用的系统接口)
get_server_time_zone() ->
	T1 = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
	T2 = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	trunc((T2 - T1) / 3600).

%% 计算服务器当前是周几 [1 - 7]
get_which_day_in_week() ->
	{{Year, Month, Day},{_Hour, _Min, _Second}} = time_impl:local_time(),
	calendar:day_of_the_week(Year, Month, Day).


next_time_interval({_, _, _} = Time) ->
	{BT, _} = CurrentTime = time_impl:local_time(),
	NextTime = {BT, Time},
	CurrentTimestamp = world_util:timestamp_3(CurrentTime),
	NextTimestamp = world_util:timestamp_3(NextTime),

	if
		NextTimestamp > CurrentTimestamp -> NextTimestamp - CurrentTimestamp;
		NextTimestamp =:= CurrentTimestamp -> 86400;
		true ->
			86400 + NextTimestamp - CurrentTimestamp
	end.



eprof(Mod, Func, Args) ->
	case eprof:start() of
		{ok, _} -> ok;
		{error, {already_started, _}} -> ok
	end,
	{ok, Ret} = eprof:profile([self()], Mod, Func, Args),
	io:format("~p~n", [Ret]),
	eprof:analyze(),
	eprof:stop().

eprof(Mod, Func, Args, Log) ->
	case eprof:start() of
		{ok, _} -> ok;
		{error, {already_started, _}} -> ok
	end,
	{ok, _} = eprof:profile([self()], Mod, Func, Args),
	ok = eprof:log(Log),
	eprof:analyze(),
	eprof:stop().

fprof(Mod, Func, Args, Log) ->
	fprof:apply(Mod, Func, Args),
	fprof:profile(),
	fprof:analyse({dest, Log}).


process_infos(Dir) ->
	filelib:ensure_dir(Dir ++ "process/"),
	File = Dir  ++ "process/" ++ world_util:time_format() ++ ".log",
	{ok, Fd} = file:open(File, [write]),
	Pids = erlang:processes(),
	lists:foreach(
		fun(Pid) ->
			ok = io:format(Fd, "~p~n", [erlang:process_info(Pid)])
		end, Pids),
	file:close(Fd).

hash(Value) when is_list(Value) ->
	hash_1(Value, 5381).

hash_1([], HashValue) -> HashValue;
hash_1([C|L], Hash) ->
	NewHash = Hash bsl 5 + Hash + C,
	hash_1(L, NewHash).


wait_stop(0) -> ok;
wait_stop(N) ->
	case init:get_status() of
		{stopping, _} ->
%%正在停止中,
			timer:sleep(500),
			wait_stop(N - 1);
		_Other -> ok
	end.

create_dir(Dir) ->
	DirItemList = string:tokens(Dir, "/"),

	create_dir_1(DirItemList, "/").

create_dir_1([], _) -> ok;
create_dir_1([DirItem|LastDir], Acc) ->
	TmpDir = filename:join([Acc, DirItem]),
	case filelib:is_dir(TmpDir) of
		true -> ok ;
		false ->
			ok = file:make_dir(TmpDir)
	end,
	create_dir_1(LastDir,TmpDir).

get_first_ip() ->
	{ok, [{ErlIp, _, _}|_]} = inet:getif(),
	ipv4_to_str(ErlIp).

get_all_ip() ->
	get_all_ip(",").


get_all_ip(Space) when is_list(Space) ->
	{ok, IpAddrList} = inet:getif(),
	IpList =
		lists:map(
			fun({ErlIp, _, _}) ->
				ipv4_to_str(ErlIp)
			end, IpAddrList),
	string:join(IpList, Space).


garbage_collect() ->
	lists:foreach(
		fun(Pid) ->
			erlang:garbage_collect(Pid)
		end, erlang:processes()).

read_uuid() ->
	{ok, Uuid} = file:read_file("/sl/uuid"),
	V = world_util:to_list(Uuid),
	[_|V1] = lists:reverse(V),
	lists:reverse(V1).


is_in_same_day(LastTimestamp, CurrentTimestamp, {Hour, Min, Sec}) when LastTimestamp =< CurrentTimestamp ->
	Offset = world_util:get_server_time_zone() * 3600,
	LocalLastTimestamp = LastTimestamp + Offset,
	LocalCurrentTimestamp = CurrentTimestamp + Offset,
	OffsetSec = Hour * 60 * 60 + Min * 60 + Sec,
	TmpLastTimestamp = LocalLastTimestamp - OffsetSec,
	TmpCurrentTimestamp = LocalCurrentTimestamp - OffsetSec,
	CurDay = world_util:get_day(TmpLastTimestamp),
	LastDay = world_util:get_day(TmpCurrentTimestamp),
	CurDay =:= LastDay.

is_in_same_week(LastTimestamp, CurrentTimestamp) when LastTimestamp =< CurrentTimestamp ->
	Offset = world_util:get_server_time_zone() * 3600,
	{{LastYear, LastMonth, LastDay}, _} = world_util:timestamp_to_datetime(LastTimestamp + Offset),
	WhichDayInWeek = calendar:day_of_the_week(LastYear, LastMonth, LastDay),
	OffsetDay = 7 - WhichDayInWeek,
	NextResetTime = world_util:timestamp_3({{LastYear, LastMonth, LastDay}, {16, 0, 0}}) + OffsetDay * 86400,
	if
		NextResetTime >= CurrentTimestamp ->
			true;
		true ->
			false
	end.

is_in_same_month(LastTimestamp, CurrentTimestamp) when LastTimestamp =< CurrentTimestamp ->
	Offset = world_util:get_server_time_zone() * 3600,
	{{LastYear, LastMonth, _}, _} = world_util:timestamp_to_datetime(LastTimestamp + Offset),
	{{CurYear, CurMonth, _}, _} = world_util:timestamp_to_datetime(CurrentTimestamp + Offset),
	if
		CurYear > LastYear ->
			false;
		CurMonth > LastMonth ->
			false;
		true ->
			true
	end.

convert_str_time_to_timestamp(StrTime) ->
	[Str1, Str2] = re:split(StrTime, " "),
	[Y, M, D] = re:split(Str1, "-"),
	[Hour, Min, Sec] = re:split(Str2, ":"),
	timestamp_3({{world_util:to_integer(Y), world_util:to_integer(M), world_util:to_integer(D)},
	             {world_util:to_integer(Hour), world_util:to_integer(Min), world_util:to_integer(Sec)}}).


param_undefined_check(Value, Reason) -> param_check(Value, undefined, Reason).
param_false_check(Value, Reason) -> param_check(Value, false, Reason).

param_check(Value, Value, Reason) ->
	?FILE_LOG_ERROR("value[~p] reason[~p]", [Value, Reason]),
	throw({custom, -1});
param_check(Value, _, _) -> Value.

generateWorldTokenId()->
	TimeStamp = timestamp(),
	StrTimeStamp =  lists:flatten(io_lib:format("~w", [TimeStamp])),
	RandomValue =  world_util:random_in_range(1, 99999999),
	StrRandomValue = lists:flatten(io_lib:format("~8..0w", [RandomValue])),
	StrFinal = StrTimeStamp ++ StrRandomValue,
	?FILE_LOG_INFO("RandomValue:~p,TimeStamp:~p,StrFinal~s",[RandomValue,TimeStamp,StrFinal]),
	FinalWorldTokenID = to_integer(StrFinal),
	FinalWorldTokenID.

send_mail(Title, Fmt, Args) ->
	V = io_lib:format(Fmt, Args),
	send_mail(Title, lists:flatten(V)).
send_mail(Title, Body) ->
	spawn(
		fun() ->
			{success, {Smtp, Acc, Pwd}} = world_config:get_cfg(mail_send_info),
			{success, ReceiveAddrList} = world_config:get_cfg(mail_receive_addr),
			{success, MailFrom} = world_config:get_cfg(mail_from),
			SelfIp = world_util:get_ip(),
			ToStr = string:join(ReceiveAddrList, "\r\nTo: "),
			SendData = io_lib:format("Subject:~s\r\nFrom:~s(~s)\r\nTo: ~s\r\n\r\n~s", [Title, binary_to_list(unicode:characters_to_binary(MailFrom)), SelfIp, ToStr, Body]),
			case gen_smtp_client:send_blocking({Acc,ReceiveAddrList,lists:flatten(SendData)},[{relay, Smtp}, {username, Acc}, {password, Pwd}]) of
				{error, Reason} ->
					?FILE_LOG_ERROR("send_mail_failed,[~p] [~p, ~p]", [Reason, Title, Body]),
					ok;
				{error, ErrorType, ErrorMessage} ->
					?FILE_LOG_ERROR("send_mail_failed,[~p, ~p] [~p, ~p]", [ErrorType, ErrorMessage, Title, Body]);
				_ -> ok
			end
		end),
	ok.

get_list_random_elem(List, N) ->
	get_list_random_elem(List, N, []).

get_list_random_elem([], _, Elems) ->
	Elems;
get_list_random_elem(_, 0, Elems) ->
	Elems;
get_list_random_elem(List, N, Elems)  ->
	Num = length(List),
	Idx = world_util:random_in_range(1, Num),
	Elem = lists:nth(Idx, List),
	NewList = lists:delete(Elem, List),
	get_list_random_elem(NewList, N - 1, [Elem| Elems]).

%%返回字符串格式的IP，多个IP之间用"-"分割
get_ip() ->
	case world_config:get_cfg(self_ip) of
		fail  ->
			{ok, AddrList} = inet:getif(),
			{IpAddrs, _} =
				lists:foldl(
					fun({Ip, _, _}, {Acc, IsAddSuffix}) ->
						if
							Ip =:= {127,0,0,1} -> {Acc, IsAddSuffix};
							true ->
								IpStr = ipv4_to_str(Ip),
								NewAcc =
									if
										IsAddSuffix =:= yes -> Acc ++ "-" ++ IpStr;
										IsAddSuffix =:= no -> Acc ++ IpStr
									end,
								{NewAcc, yes}
						end
					end, {"", no}, AddrList),
			world_config:write_cfg(self_ip, IpAddrs),
			IpAddrs;
		{success, V} -> V
	end.

get_name_length(Name) ->
	L = unicode:characters_to_list(list_to_binary(Name)),

	lists:foldl(fun(E, TmpLength) ->
		if
			E =< 255 ->
				TmpLength + 1;
			E >= 16#4e00 andalso E =< 16#9fff ->
				TmpLength + 2;
			true ->
				TmpLength + 1
		end
	            end, 0, L).


%%检测2个时间戳相对于某个时间是否在同一天                         Z
check_timestamp(LastTimestamp, CurrentTimestamp, {Hour, Min, Sec}) when LastTimestamp =< CurrentTimestamp ->
	Offset = world_util:get_server_time_zone() * 3600,
	LocalLastTimestamp = LastTimestamp + Offset,
	LocalCurrentTimestamp = CurrentTimestamp + Offset,
	OffsetSec = Hour * 60 * 60 + Min * 60 + Sec,
	TmpLastTimestamp = LocalLastTimestamp - OffsetSec,
	TmpCurrentTimestamp = LocalCurrentTimestamp - OffsetSec,
	CurDay = world_util:get_day(TmpLastTimestamp),
	LastDay = world_util:get_day(TmpCurrentTimestamp),
	CurDay =:= LastDay.

%%检测2个时间戳相对于某个时间是否在同一周
check_timestamp_week(LastTimestamp, CurrentTimestamp, {Hour, Min, Sec}) when LastTimestamp =< CurrentTimestamp ->
	Offset = world_util:get_server_time_zone() * 3600,
	LocalLastTimestamp = LastTimestamp + Offset,
	LocalCurrentTimestamp = CurrentTimestamp + Offset,
	OffsetSec = Hour * 60 * 60 + Min * 60 + Sec,
	TmpLastTimestamp = LocalLastTimestamp - OffsetSec,
	TmpCurrentTimestamp = LocalCurrentTimestamp - OffsetSec,

	{{LastYear, LastMonth, LastDay}, {LastHour, LastMinute, LastSecond}} = world_util:timestamp_to_datetime(TmpLastTimestamp),
	{{CurrentYear, CurrentMonth, CurrentDay}, _} = world_util:timestamp_to_datetime(TmpCurrentTimestamp),
	LastWeek = calendar:day_of_the_week(LastYear, LastMonth, LastDay),
	CurrentWeek = calendar:day_of_the_week(CurrentYear, CurrentMonth, CurrentDay),
	if
		CurrentWeek < LastWeek -> false;         %%肯定不在同一周了
		true ->
			%%看相隔天数
			IntervalDay = 7 - LastWeek,
			%%下周一0:0:0的时间戳
			NextWeekTimestamp = TmpLastTimestamp + (60 - LastSecond) + (59 - LastMinute) * 60 + (23-LastHour) * 60 * 60 + IntervalDay * 86400,
			if
				TmpCurrentTimestamp >= NextWeekTimestamp -> false;
				true -> true
			end

	end.




proc_mem_all(LimitKb) ->
	Procs = [{undefined, Pid} || Pid<- erlang:processes()],
	proc_mem_all(Procs, LimitKb).

proc_mem_all(Proces, LimitKb) ->
	SizeLimit = LimitKb * 1024,
	WordSize = erlang:system_info(wordsize),
	{R, Total} =
		lists:foldl(
			fun({Name, Pid}, {Acc, TotalSize}) ->
				case erlang:process_info(Pid, total_heap_size) of
					{_, Size0} ->
						Size = Size0 * WordSize,
						case Size >= SizeLimit of
							true -> {[{Name, Pid, Size} | Acc], TotalSize+Size};
							false -> {Acc, TotalSize}
						end;
					_ -> {Acc, TotalSize}
				end
			end, {[], 0}, Proces),
	R1 = lists:keysort(3, R),
	{Total, lists:reverse(R1)}.
