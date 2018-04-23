package main

import (
	"crypto/md5"
	"encoding/xml"
	"fmt"
	_ "github.com/go-sql-driver/MySQL"
	"github.com/natefinch/lumberjack"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"time"
)

func main() {
	// read args
	if len(os.Args) < 2 {
		log.Fatal("./recharge configFile")
	}
	// read configuration
	configXML, err := ioutil.ReadFile(os.Args[1])
	if err != nil {
		log.Fatal("Can't open config-file ", err)
	}
	err = xml.Unmarshal(configXML, &configGlobal)
	if err != nil {
		log.Fatal("Can't unmarshal config-file ", err)
	}

	loadServerList()
	// redirect to log system
	log.SetOutput(&lumberjack.Logger{
		Filename:   configGlobal.LogFile,
		MaxSize:    20,
		MaxBackups: 9,
		MaxAge:     28,
		Compress:   false,
	})
	log.Println("NOW: ", time.Now().Unix())

	initDB()

	go startServerListLoadTimer()

	// start listening
	http.HandleFunc("/proxy_v2_recharge", v2Recharge)
	http.HandleFunc("/gaeapay", gaeaRecharge)
	listenAddr := configGlobal.ListenIp + ":" + configGlobal.ListenPort
	httpServer := &http.Server{Addr: listenAddr, Handler: nil}
	if configGlobal.IsHttps == "true" {
		log.Println("Start recharge server (HTTPS), listen addr: ", listenAddr)
		err = httpServer.ListenAndServeTLS(configGlobal.CertFile, configGlobal.KeyFile)
	} else {
		log.Println("Start recharge server (HTTP), listen addr: ", listenAddr)
		err = httpServer.ListenAndServe()
	}
	if err != nil {
		log.Fatal("ListenAndServeTLS: ", err)
	}
}

// V2 Recharge
type v2RechargeStruct struct {
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

func (v2 v2RechargeStruct) check() bool {
	basicList := []string{v2.hjOrderId, v2.hjOrderId, v2.hjUniqueId, v2.hjAppId, v2.hjUserId, v2.hjRoleId, v2.hjServerId, v2.hjOrderTime, v2.hjChannel, v2.hjAmount, v2.hjCurrency, v2.hjItemId, v2.hjItemName, v2.hjPayExt, v2.hjVersion, configGlobal.V2SecretKey}
	var basic string
	for _, v := range basicList {
		if v != "" {
			basic += v + "#"
		}
	}
	return md5Local(basic) == v2.hjSign
}

func (v2 v2RechargeStruct) string() string {
	return fmt.Sprintln(
		"&hjOrderId:"+v2.hjOrderId,
		"&hjUniqueId:"+v2.hjUniqueId,
		"&hjAppId:"+v2.hjAppId,
		"&hjUserId:"+v2.hjUserId,
		"&hjRoleId:"+v2.hjRoleId,
		"&hjServerId:"+v2.hjServerId,
		"&hjOrderTime:"+v2.hjOrderTime,
		"&hjChannel:"+v2.hjChannel,
		"&hjAmount:"+v2.hjAmount,
		"&hjCurrency:"+v2.hjCurrency,
		"&hjItemId:"+v2.hjItemId,
		"&hjItemName:"+v2.hjItemName,
		"&hjPayExt:"+v2.hjPayExt,
		"&hjVersion:"+v2.hjVersion,
		"&hjSign:"+v2.hjSign,
	)
}

func v2Recharge(rsp http.ResponseWriter, req *http.Request) {
	if req.Method != "POST" {
		log.Println("Bad http method")
		return
	}
	err := req.ParseForm()
	if err != nil {
		log.Println("Can't ParseForm: ", err)
		return
	}
	info := v2RechargeStruct{
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

	if info.check() == false {
		log.Println("Authorize Fail")
		return
	}

	serverRsp, err := http.PostForm(getServer(info.hjServerId)+"/proxy_v2_recharge", req.PostForm)
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

// Gaea Recharge
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
	var basic string
	for _, v := range basicList {
		if v != "" {
			basic += v
		}
	}
	return md5Local(basic) == gaea.signature
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

	if info.check() == false {
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
		channel:  "Gaea",
		serverId: info.serverId,
		orderId:  info.orderId,
		appId:    info.appId,
		ext:      info.string(),
	}
	log.write()
}

// md5
func md5Local(basic string) string {
	h := md5.New()
	h.Write([]byte(basic))
	return string(h.Sum(nil))
}
