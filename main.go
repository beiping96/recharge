package main

import (
	"database/sql"
	"encoding/xml"
	"fmt"
	_ "github.com/go-sql-driver/MySQL"
	"github.com/natefinch/lumberjack"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"os"
	"sync"
	"time"
)

// load server list each 10mins
const interval = 10

type Server struct {
	Id  string `xml:"id"`
	Url string `xml:"url"`
}

var serverList map[string]string
var lock sync.RWMutex

type Config struct {
	ListenIp          string `xml:"listen_ip"`
	ListenPort        string `xml:"listen_port"`
	IsHttps           string `xml:"is_https"`
	CertFile          string `xml:"cert_file"`
	KeyFile           string `xml:"key_file"`
	MysqlIp           string `xml:"mysql_ip"`
	MysqlPort         string `xml:"mysql_port"`
	MysqlUser         string `xml:"mysql_user"`
	MysqlPw           string `xml:"mysql_password"`
	MysqlDB           string `xml:"mysql_db"`
	ServerListXMLFile string `xml:"server_list_xml_file"`
	LogFile           string `xml:"log_file"`
}

var configGlobal Config

type logStruct struct {
	channel  string
	serverId string
	orderId  string
	appId    string
	ext      string
}

var logChan chan logStruct

const dbConnectionCount = 2

func (logItem logStruct) write() {
	logChan <- logItem
}

func init() {
	serverList = make(map[string]string)
	logChan = make(chan logStruct, dbConnectionCount)
}

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
	// redirect the log system
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

type V2RechargeStruct struct {
	HJOrderId   string
	HJUniqueId  string
	HJAppId     string
	HJUserId    string
	HJRoleId    string
	HJServerId  string
	HJOrderTime string
	HJChannel   string
	HJAmount    string
	HJCurrency  string
	HJItemId    string
	HJItemName  string
	HJPayExt    string
	HJVersion   string
	HJSign      string
}

func (v2 V2RechargeStruct) check() bool {
	return false
}

func (v2 V2RechargeStruct) string() string {
	return ""
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
	info := V2RechargeStruct{
		HJOrderId:   req.PostFormValue("HJOrderId"),
		HJUniqueId:  req.PostFormValue("HJUniqueId"),
		HJAppId:     req.PostFormValue("HJAppId"),
		HJUserId:    req.PostFormValue("HJUserId"),
		HJRoleId:    req.PostFormValue("HJRoleId"),
		HJServerId:  req.PostFormValue("HJServerId"),
		HJOrderTime: req.PostFormValue("HJOrderTime"),
		HJChannel:   req.PostFormValue("HJChannel"),
		HJAmount:    req.PostFormValue("HJAmount"),
		HJCurrency:  req.PostFormValue("HJCurrency"),
		HJItemId:    req.PostFormValue("HJItemId"),
		HJItemName:  req.PostFormValue("HJItemName"),
		HJPayExt:    req.PostFormValue("HJPayExt"),
		HJVersion:   req.PostFormValue("HJVersion"),
		HJSign:      req.PostFormValue("HJSign"),
	}

	if info.check() == false {
		log.Println("Authorize Fail")
		return
	}

	serverRsp, err := http.PostForm(getServer(info.HJServerId)+"/proxy_v2_recharge", req.PostForm)
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
		serverId: info.HJServerId,
		orderId:  info.HJOrderId,
		appId:    info.HJAppId,
		ext:      info.string(),
	}
	log.write()
}

type GaeaRechargeStruct struct {
	AppId        string
	ServerId     string
	Uid          string
	Amount       string
	OrderId      string
	ItemId       string
	ActualAmount string
	PayExt       string
	Currency     string
	Signature    string
}

func (gaea GaeaRechargeStruct) check() bool {
	return false
}

func (gaea GaeaRechargeStruct) string() string {
	return ""
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

	info := GaeaRechargeStruct{
		AppId:        req.FormValue("appid"),
		ServerId:     req.FormValue("serverid"),
		Uid:          req.FormValue("uid"),
		Amount:       req.FormValue("amount"),
		OrderId:      req.FormValue("orderid"),
		ItemId:       req.FormValue("item"),
		ActualAmount: req.FormValue("actual_amount"),
		PayExt:       req.FormValue("payext"),
		Currency:     req.FormValue("currency"),
		Signature:    req.FormValue("signature"),
	}

	if info.check() == false {
		log.Println("Authorize Fail")
		return
	}
	postForm := url.Values{
		"appid":         {info.AppId},
		"serverid":      {info.ServerId},
		"uid":           {info.Uid},
		"amount":        {info.Amount},
		"orderid":       {info.OrderId},
		"item":          {info.ItemId},
		"actual_amount": {info.ActualAmount},
		"payext":        {info.PayExt},
		"currency":      {info.Currency},
		"signature":     {info.Signature},
	}
	serverRsp, err := http.PostForm(getServer(info.ServerId)+"/proxy_gaeapay_recharge", postForm)
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
		serverId: info.ServerId,
		orderId:  info.OrderId,
		appId:    info.AppId,
		ext:      info.string(),
	}
	log.write()
}

func initDB() {
	connStr := fmt.Sprintf("%s:%s@tcp(%s:%s)/%s?charset=utf8",
		configGlobal.MysqlUser, configGlobal.MysqlPw, configGlobal.MysqlIp, configGlobal.MysqlPort, configGlobal.MysqlDB)
	// create connection
	for i := 0; i < dbConnectionCount; i++ {
		db, err := sql.Open("mysql", connStr)
		if err != nil {
			log.Fatal("SQL Open: ", err)
		}
		err = db.Ping()
		if err != nil {
			log.Fatal("SQL Ping: ", err)
		}
		go dbWorker(db)
	}
}

func dbWorker(db *sql.DB) {
	defer db.Close()
	stmt, err := db.Prepare("INSERT INTO recharge_log (unix, server_id, channel, order_id, app_id, ext) values (?, '?', '?', '?', '?', '?')")
	if err != nil {
		log.Fatal("SQL Exec: ", err)
	}
	for {
		logOne := <-logChan
		_, err := stmt.Exec(time.Now().UTC().Unix(), logOne.serverId, logOne.channel, logOne.orderId, logOne.appId, logOne.ext)
		if err != nil {
			log.Println("SQL Exec: ", err, "INFO: ", logOne)
		}
	}
}

func getServer(serverId string) string {
	lock.RLock()
	defer lock.RUnlock()
	return serverList[serverId]
}

func startServerListLoadTimer() {
	tick := time.NewTicker(time.Duration(interval) * time.Minute)
	for {
		select {
		case <-tick.C:
			loadServerList()
		}
	}
}

func loadServerList() {
	serverListXML, err := ioutil.ReadFile(configGlobal.ServerListXMLFile)
	if err != nil {
		log.Fatal("Can't open ServerList confie file: ", err)
	}
	var serverS []Server
	err = xml.Unmarshal(serverListXML, &serverS)
	if err != nil {
		log.Fatal("Can't unmarshal ServerList confie file: ", err)
	}
	for _, server := range serverS {
		saveServer(server)
	}
}

func saveServer(server Server) {
	lock.Lock()
	defer lock.Unlock()
	serverList[server.Id] = server.Url
}
