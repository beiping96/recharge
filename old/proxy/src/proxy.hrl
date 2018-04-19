%% return string()
-define(PROXY_VERSION, element(2, application:get_key(proxy, vsn))).
-define(PROXY_DESCRIPTION, element(2, application:get_key(proxy, description))).

-define(POST, 'POST').
-define(GET, 'GET').

-define(ZB_SIGN_ERR, 101).
-define(ZB_SYS_ERR, 102).
-define(ZB_ORDER_NOT_EXIST, 103).   %%订单不存在

-define(ORDER_SAVE_LIMIT, 5).
-define(ANDROID_RECHARGE_PRIVATE_KEY, "tjc20166EJxj").
-define(IOS_RECHARGE_PRIVATE_KEY, "13dd5813757bc86c").
-define(IOS_GAEA_RECHARGE_CALLBACK_KEY, "107hyl507habea4e").