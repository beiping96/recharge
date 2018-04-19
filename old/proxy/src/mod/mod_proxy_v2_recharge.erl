%%%-------------------------------------------------------------------
%%% @author yaohong
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 十一月 2016 10:29
%%%-------------------------------------------------------------------
-module(mod_proxy_v2_recharge).
-author("yaohong").

-compile(export_all).
%% API
-include("../../deps/file_log/include/file_log.hrl").
-include("../proxy.hrl").

%% API
-export([req_handle/1]).

req_handle(Req) ->

    world_util:param_false_check((Req:get(method) =:= ?POST), "request method error!"),

    PostData = Req:parse_post(),
    ?FILE_LOG_DEBUG("post_data=~p", [PostData]),


    HJOrderId = get_value("HJOrderId", PostData),
    HJUniqueId = get_value("HJUniqueId", PostData),
    HJAppId = get_value("HJAppId", PostData),
    HJUserId = get_value("HJUserId", PostData),
    HJRoleId = get_value("HJRoleId", PostData),
    HJServerId = get_value("HJServerId", PostData),
    HJOrderTime = get_value("HJOrderTime", PostData),
    HJChannel = get_value("HJChannel", PostData),
    HJAmount = get_value("HJAmount", PostData),
    HJCurrency = get_value("HJCurrency", PostData),
    HJItemId = get_value("HJItemId", PostData),
    HJItemName = get_value("HJItemName", PostData),
    HJPayExt = get_value("HJPayExt", PostData),
    HJVersion = get_value("HJVersion", PostData),
    HJSign = get_value("HJSign", PostData),


    verify_order(HJOrderId, HJUniqueId, HJAppId, HJUserId, HJRoleId, HJServerId, HJOrderTime, HJChannel, HJAmount, HJCurrency, HJItemId, HJItemName, HJPayExt, HJVersion, HJSign),


    {_, ServerId} = HJServerId,
    {success, RechargeCallbackUrl} = proxy_server_mgr:get_callback_url(ServerId),
%%发送给world充值
    {ok, {{_, 200, _}, _, JsonData}} =
        httpc:request(
            'post',
            {
                    RechargeCallbackUrl ++ "/proxy_v2_recharge", [],
                "application/x-www-form-urlencoded",
                Req:recv_body()},
            [], []),
    JsonData.



get_value(Key, PostData) ->
    {Key, proplists:get_value(Key, PostData)}.


verify_order(HJOrderId, HJUniqueId, HJAppId, HJUserId, HJRoleId, HJServerId, HJOrderTime, HJChannel, HJAmount, HJCurrency, HJItemId, HJItemName, HJPayExt, HJVersion, {_, HJSign}) ->
    SignStr = convert_sign_string(HJOrderId, HJUniqueId, HJAppId, HJUserId, HJRoleId, HJServerId, HJOrderTime, HJChannel, HJAmount, HJCurrency, HJItemId, HJItemName, HJPayExt, HJVersion) ++ ?ANDROID_RECHARGE_PRIVATE_KEY,
    Md5String = lists:flatten([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(SignStr))]),
    if
        HJSign =:= Md5String ->
            success;
        true ->
            ?FILE_LOG_ERROR("HJSign is ~p, Md5String is ~p", [HJSign, Md5String]),
            throw({custom, ?ZB_SIGN_ERR})
    end.


convert_sign_string(HJOrderId, HJUniqueId, HJAppId, HJUserId, HJRoleId, HJServerId, HJOrderTime, HJChannel, HJAmount, HJCurrency, HJItemId, HJItemName, HJPayExt, HJVersion) ->
    %%过滤为null
    FilterList = lists:filter(
        fun({_Key, Value}) ->
            if
                Value == undefined ->
                    false;
                true ->
                    true
            end
        end, [HJOrderId, HJUniqueId, HJAppId, HJUserId, HJRoleId, HJServerId, HJOrderTime, HJChannel, HJAmount, HJCurrency, HJItemId, HJItemName, HJPayExt, HJVersion]),
    SortList = world_util:eksort(FilterList),
    ?FILE_LOG_DEBUG("SortList is ~p", [SortList]),
    lists:foldl(
        fun({_, Value}, TmpStr) ->
            TmpStr ++ world_util:to_list(Value) ++ "#"
        end, "", SortList).