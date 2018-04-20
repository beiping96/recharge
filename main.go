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
	"net"
	"net/http"
	"net/url"
	"os"
	"strconv"
	"strings"
	"sync"
	"time"
)

// load server list each 10mins
const interval = 10

type server struct {
	id   string `xml:id`
	ip   string `xml:ip`
	port string `xml:port`
}

var serverList map[string]server
var lock sync.RWMutex

type config struct {
	listenIp          string `xml:listen_ip`
	listenPort        string `xml:listen_port`
	isHttps           string `xml:is_https`
	certFile          string `xml:cert_file`
	keyFile           string `xml:key_file`
	mysqlIp           string `xml:mysql_ip`
	mysqlPort         string `xml:mysql_port`
	mysqlUser         string `xml:mysql_user`
	mysqlPw           string `xml:mysql_password`
	mysqlDB           string `xml:mysql_db`
	serverListXMLFile string `xml:server_list_xml_file`
	logFile           string `xml:log_file`
}

var configGlobal config

var logChan chan string

const dbConnection = 8

func init() {
	logChan = make(chan string, dbConnection)
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
		Filename:   configGlobal.logFile,
		MaxSize:    20,
		MaxBackups: 9,
		MaxAge:     28,
		Compress:   false,
	})
	log.Println(time.Now().Unix())

	go startServerListLoadTimer()

	// start listening
	http.HandleFunc("/proxy_v2_recharge", v2Recharge)
	http.HandleFunc("/gaeapay", gaeaRecharge)
	listenAddr := configGlobal.listenIp + ":" + configGlobal.listenPort
	httpServer := &http.Server{Addr: listenAddr, Handler: nil}
	if configGlobal.isHttps == "true" {
		err = httpServer.ListenAndServeTLS(configGlobal.certFile, configGlobal.keyFile)
	} else {
		err = httpServer.ListenAndServe()
	}
	if err != nil {
		log.Fatal("ListenAndServeTLS: ", err)
	}
	log.Println("Start recharge server success, listen addr: ", listenAddr)
}

func v2Recharge(http.ResponseWriter, *http.Request) {

}

func gaeaRecharge(http.ResponseWriter, *http.Request) {

}

func writeDB()

func getServer(serverId string) server {
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
	serverListXML, err := ioutil.ReadFile(configGlobal.serverListXMLFile)
	if err != nil {
		log.Fatal(err)
	}
	var serverS []server
	err = xml.Unmarshal(serverListXML, &serverS)
	if err != nil {
		log.Fatal(err)
	}
	for _, serverOne := range serverS {
		saveServer(serverOne)
	}
}

func saveServer(serverOne server) {
	lock.Lock()
	defer lock.Unlock()
	serverList[serverOne.id] = serverOne
}
