package main

import (
	// "database/sql"
	// "encoding/json"
	"encoding/xml"
	// "fmt"
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

var logChan chan string

const dbConnectionCount = 8

func init() {
	serverList = make(map[string]Server)
	logChan = make(chan string, dbConnectionCount)
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

func v2Recharge(http.ResponseWriter, *http.Request) {

}

func gaeaRecharge(http.ResponseWriter, *http.Request) {

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
