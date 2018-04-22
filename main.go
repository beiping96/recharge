package main

import (
	"database/sql"
	"encoding/json"
	"encoding/xml"
	"fmt"
	_ "github.com/go-sql-driver/MySQL"
	"github.com/natefinch/lumberjack"
	"io/ioutil"
	"log"
	// "net"
	"net/http"
	// "net/url"
	"os"
	// "strconv"
	// "strings"
	"sync"
	"time"
)

// load server list each 10mins
const interval = 10

type Server struct {
	Id   string `xml:"id"`
	Ip   string `xml:"ip"`
	Port string `xml:"port"`
}

var serverList map[string]Server
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
	serverList = make(map[string]Server)
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
		log.Fatal(err)
	}
	err = xml.Unmarshal(configXML, &configGlobal)
	if err != nil {
		log.Fatal(err)
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
	log.Println(time.Now().Unix())

	initDB()

	go startServerListLoadTimer()

	// start listening
	http.HandleFunc("/proxy_v2_recharge", v2Recharge)
	http.HandleFunc("/gaeapay", gaeaRecharge)
	listenAddr := configGlobal.ListenIp + ":" + configGlobal.ListenPort
	httpServer := &http.Server{Addr: listenAddr, Handler: nil}
	if configGlobal.IsHttps == "true" {
		log.Println("Start recharge server (HTTP), listen addr: ", listenAddr)
		err = httpServer.ListenAndServeTLS(configGlobal.CertFile, configGlobal.KeyFile)
	} else {
		log.Println("Start recharge server (HTTPS), listen addr: ", listenAddr)
		err = httpServer.ListenAndServe()
	}
	if err != nil {
		log.Fatal("ListenAndServeTLS: ", err)
	}
}

type V2RechargeStruct struct {
	HJOrderId   string `json:"HJOrderId"`
	HJUniqueId  string `json:"HJUniqueId"`
	HJAppId     string `json:"HJAppId"`
	HJUserId    string `json:"HJUserId"`
	HJRoleId    string `json:"HJRoleId"`
	HJServerId  string `json:"HJServerId"`
	HJOrderTime string `json:"HJOrderTime"`
	HJChannel   string `json:"HJChannel"`
	HJAmount    string `json:"HJAmount"`
	HJCurrency  string `json:"HJCurrency"`
	HJItemId    string `json:"HJItemId"`
	HJItemName  string `json:"HJItemName"`
	HJPayExt    string `json:"HJPayExt"`
	HJVersion   string `json:"HJVersion"`
	HJSign      string `json:"HJSign"`
}

func v2Recharge(rsp http.ResponseWriter, req *http.Request) {
	var post V2RechargeStruct
	json.Unmarshal(req.PostForm, &post)

	log := logStruct{
		channel:  "HJ",
		serverId: post.HJServerId,
		orderId:  post.HJOrderId,
		appId:    post.HJAppId,
		ext:      post.String(),
	}
	log.write()
}

type GaeaRechargeStruct struct {
	AppId        string `json:"appid"`
	ServerId     string `json:"serverid"`
	Uid          string `json:"uid"`
	Amount       string `json:"amount"`
	OrderId      string `json:"orderid"`
	ItemId       string `json:"item"`
	ActualAmount string `json:"actual_amount"`
	PayExt       string `json:"payext"`
	Currency     string `json:"currency"`
	Signature    string `json:"signature"`
}

func gaeaRecharge(rsp http.ResponseWriter, req *http.Request) {
	var post GaeaRechargeStruct
	json.Unmarshal(req.PostForm, &post)

	log := logStruct{
		channel:  "Gaea",
		serverId: post.ServerId,
		orderId:  post.OrderId,
		appId:    post.AppId,
		ext:      post.String(),
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
			log.Fatal(err)
		}
		err = db.Ping()
		if err != nil {
			log.Fatal(err)
		}
		go dbWorker(db)
	}
}

func dbWorker(db *sql.DB) {
	for {
		log := <-logChan
		sql := "
INSERT INTO log_recharge (time, )
		"
		sql := fmt.Sprintf("insert into content (zone_id, zone_cont) values (%v, '%v')", log.zone_id, content)
		db.Exec(sql)
	}
}

func getServer(serverId string) Server {
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
		log.Fatal(err)
	}
	var serverS []Server
	err = xml.Unmarshal(serverListXML, &serverS)
	if err != nil {
		log.Fatal(err)
	}
	for _, server := range serverS {
		saveServer(server)
	}
}

func saveServer(server Server) {
	lock.Lock()
	defer lock.Unlock()
	serverList[server.Id] = server
}
