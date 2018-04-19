%%%-------------------------------------------------------------------
%%% @author mayday
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. 十二月 2017 17:22
%%%-------------------------------------------------------------------
-module(mod_proxy_gaea_recharge).
-author("mayday").

-compile(export_all).

%% API
-include("../../deps/file_log/include/file_log.hrl").
-include("../proxy.hrl").
-export([req_handle/1]).

req_handle(Req) ->
    world_util:param_false_check((Req:get(method) =:= ?GET), "request method error!"),
    PostData = Req:parse_qs(),
    ?FILE_LOG_DEBUG("post_data is ~p", [PostData]),

    AppId = get_value("appid", PostData),
    ParamServerId = get_value("serverid", PostData),
    Uid = get_value("uid", PostData),
    Amount = get_value("amount", PostData),
    OrderId = get_value("orderid", PostData),
    ItemId = get_value("item", PostData),
    ActualAmount = get_value("actual_amount", PostData),
    PayExt = get_value("payext", PostData),
    Currency = get_value("currency", PostData),
    Signature = get_value("signature", PostData),

    verify_order(AppId, ParamServerId, Uid, Amount, OrderId, ItemId, ActualAmount, PayExt, Currency, Signature),

    {_, ServerId} = ParamServerId,
    {success, RechargeCallbackUrl} = proxy_server_mgr:get_callback_url(ServerId),
    Params = relax_util:urlencode(PostData),
    %%发送给world充值
    {ok, {{_, 200, _}, _, JsonData}} =
        httpc:request(
            'post',
            {
                    RechargeCallbackUrl ++ "/proxy_gaeapay_recharge", [],
                    "application/x-www-form-urlencoded",
                    Params},
            [], []),
    JsonData.

get_value(Key, PostData) ->
    {Key, proplists:get_value(Key, PostData)}.

verify_order(AppId, ServerId, Uid, Amount, OrderId, ItemId, ActualAmount, PayExt, Currency, {_, Sign}) ->
    TmpParamStr = lists:foldr(fun({_K, V}, TmpStr) ->
        if
            V == undefined ->
                TmpStr;
            true ->
                V ++ TmpStr
        end
    end, "", [AppId, ServerId, Uid, Amount, OrderId, ItemId, ActualAmount, PayExt, Currency]),
    ParamStr = TmpParamStr ++ ?IOS_GAEA_RECHARGE_CALLBACK_KEY,
    Md5Str = lists:flatten([io_lib:format("~2.16.0b", [D]) || D <- binary_to_list(erlang:md5(ParamStr))]),
    if
        Sign =:= Md5Str ->
            success;
        true ->
            ?FILE_LOG_ERROR("Signature = ~p, MD5 = ~p", [Sign, Md5Str]),
            throw({custom, ?ZB_SIGN_ERR})
    end.
