// HJ Recharge
package main

import (
	"crypto/md5"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
)

type HJRechargeStruct struct {
	hjOrderId   string
	hjUniqueId  string
	hjAppId     string
	hjUserId    string
	hjRoleId    string
	hjServerId  string
	hjOrderTime string
	hjChannel   string
	hjAmount    string
	hjCurrency  string
	hjItemId    string
	hjItemName  string
	hjPayExt    string
	hjVersion   string
	hjSign      string
}

func (hj HJRechargeStruct) check() bool {
	basicList := []string{hj.hjOrderId, hj.hjOrderId, hj.hjUniqueId, hj.hjAppId, hj.hjUserId, hj.hjRoleId, hj.hjServerId, hj.hjOrderTime, hj.hjChannel, hj.hjAmount, hj.hjCurrency, hj.hjItemId, hj.hjItemName, hj.hjPayExt, hj.hjVersion, configGlobal.HJSecretKey}
	basic := md5.New()
	for _, v := range basicList {
		if v != "" {
			basic.Write([]byte(v + "#"))
		}
	}
	return string(basic.Sum(nil)) == hj.hjSign
}

func (hj HJRechargeStruct) string() string {
	return fmt.Sprintln(
		"&hjOrderId:"+hj.hjOrderId,
		"&hjUniqueId:"+hj.hjUniqueId,
		"&hjAppId:"+hj.hjAppId,
		"&hjUserId:"+hj.hjUserId,
		"&hjRoleId:"+hj.hjRoleId,
		"&hjServerId:"+hj.hjServerId,
		"&hjOrderTime:"+hj.hjOrderTime,
		"&hjChannel:"+hj.hjChannel,
		"&hjAmount:"+hj.hjAmount,
		"&hjCurrency:"+hj.hjCurrency,
		"&hjItemId:"+hj.hjItemId,
		"&hjItemName:"+hj.hjItemName,
		"&hjPayExt:"+hj.hjPayExt,
		"&hjVersion:"+hj.hjVersion,
		"&hjSign:"+hj.hjSign,
	)
}

func hjRecharge(rsp http.ResponseWriter, req *http.Request) {
	if req.Method != "POST" {
		log.Println("Bad http method")
		return
	}
	err := req.ParseForm()
	if err != nil {
		log.Println("Can't ParseForm: ", err)
		return
	}
	info := HJRechargeStruct{
		hjOrderId:   req.PostFormValue("HJOrderId"),
		hjUniqueId:  req.PostFormValue("HJUniqueId"),
		hjAppId:     req.PostFormValue("HJAppId"),
		hjUserId:    req.PostFormValue("HJUserId"),
		hjRoleId:    req.PostFormValue("HJRoleId"),
		hjServerId:  req.PostFormValue("HJServerId"),
		hjOrderTime: req.PostFormValue("HJOrderTime"),
		hjChannel:   req.PostFormValue("HJChannel"),
		hjAmount:    req.PostFormValue("HJAmount"),
		hjCurrency:  req.PostFormValue("HJCurrency"),
		hjItemId:    req.PostFormValue("HJItemId"),
		hjItemName:  req.PostFormValue("HJItemName"),
		hjPayExt:    req.PostFormValue("HJPayExt"),
		hjVersion:   req.PostFormValue("HJVersion"),
		hjSign:      req.PostFormValue("HJSign"),
	}

	if configGlobal.Mode != "debug" && info.check() == false {
		log.Println("Authorize Fail")
		return
	}

	serverRsp, err := http.PostForm(getServer(info.hjServerId)+"/proxy_hj_recharge", req.PostForm)
	if err != nil {
		log.Println("Callback Error: ", err)
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
		channel:  "HJ",
		serverId: info.hjServerId,
		orderId:  info.hjOrderId,
		appId:    info.hjAppId,
		ext:      info.string(),
	}
	log.write()
}
