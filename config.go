package main

import (
	"encoding/xml"
	"io/ioutil"
	"log"
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
	V2SecretKey       string `xml:"v2_secret_key"`
	GaeaSecretKey     string `xml:"gaea_secret_key"`
}

var configGlobal Config

func init() {
	serverList = make(map[string]string)
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
