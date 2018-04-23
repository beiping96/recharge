// Gaea Recharge
package main

import (
	"crypto/md5"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
)

type gaeaRechargeStruct struct {
	appId        string
	serverId     string
	uId          string
	amount       string
	orderId      string
	itemId       string
	actualAmount string
	payExt       string
	currency     string
	signature    string
}

func (gaea gaeaRechargeStruct) check() bool {
	basicList := []string{gaea.appId, gaea.serverId, gaea.uId, gaea.amount, gaea.orderId, gaea.itemId, gaea.actualAmount, gaea.payExt, gaea.currency, configGlobal.GaeaSecretKey}
	basic := md5.New()
	for _, v := range basicList {
		if v != "" {
			basic.Write([]byte(v))
		}
	}
	return string(basic.Sum(nil)) == gaea.signature
}

func (gaea gaeaRechargeStruct) string() string {
	return fmt.Sprintln(
		"&appId:"+gaea.appId,
		"&serverId:"+gaea.serverId,
		"&uId:"+gaea.uId,
		"&amount:"+gaea.amount,
		"&orderId:"+gaea.orderId,
		"&itemId:"+gaea.itemId,
		"&actualAmount:"+gaea.actualAmount,
		"&payExt:"+gaea.payExt,
		"&currency:"+gaea.currency,
		"&signature:"+gaea.signature,
	)
}

func gaeaRecharge(rsp http.ResponseWriter, req *http.Request) {
	if req.Method != "GET" {
		log.Println("Bad http method")
		return
	}
	err := req.ParseForm()
	if err != nil {
		log.Println("Can't ParseForm: ", err)
		return
	}

	info := gaeaRechargeStruct{
		appId:        req.FormValue("appid"),
		serverId:     req.FormValue("serverid"),
		uId:          req.FormValue("uid"),
		amount:       req.FormValue("amount"),
		orderId:      req.FormValue("orderid"),
		itemId:       req.FormValue("item"),
		actualAmount: req.FormValue("actual_amount"),
		payExt:       req.FormValue("payext"),
		currency:     req.FormValue("currency"),
		signature:    req.FormValue("signature"),
	}

	if configGlobal.Mode != "debug" && info.check() == false {
		log.Println("Authorize Fail")
		return
	}
	postForm := url.Values{
		"appid":         {info.appId},
		"serverid":      {info.serverId},
		"uid":           {info.uId},
		"amount":        {info.amount},
		"orderid":       {info.orderId},
		"item":          {info.itemId},
		"actual_amount": {info.actualAmount},
		"payext":        {info.payExt},
		"currency":      {info.currency},
		"signature":     {info.signature},
	}
	serverRsp, err := http.PostForm(getServer(info.serverId)+"/proxy_gaeapay_recharge", postForm)
	if err != nil {
		log.Println("Try Callback url: ", getServer(info.serverId), " Error: ", err)
		return
	}

	defer serverRsp.Body.Close()
	body, err := ioutil.ReadAll(serverRsp.Body)
	if err != nil {
		log.Println("Callback body: ", err)
		return
	}
	rsp.Write(body)
	rsp.WriteHeader(serverRsp.StatusCode)

	log := logStruct{
		channel:  "Gaea",
		serverId: info.serverId,
		orderId:  info.orderId,
		appId:    info.appId,
		ext:      info.string(),
	}
	log.write()
}
