// Load global config and serverlist
package main

import (
	"encoding/base64"
	"encoding/xml"
	"io/ioutil"
	"log"
	"net/http"
	"strings"
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
	Mode              string `xml:"mode"`
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
	LogFile           string `xml:"log_file"`
	ServerListLoadUrl string `xml:"server_list_load_url"`
	HttpAuth          string `xml:"http_auth"`
	HttpPW            string `xml:"http_pw"`
	ServerListXMLFile string `xml:"server_list_xml_file"`
	HJSecretKey       string `xml:"hj_secret_key"`
	GaeaSecretKey     string `xml:"gaea_secret_key"`
}

var configGlobal Config

func init() {
	serverList = make(map[string]string)
}

func checkAuth(req *http.Request) bool {
	s := strings.SplitN(req.Header.Get("Authorization"), " ", 2)
	if len(s) != 2 {
		return false
	}

	b, err := base64.StdEncoding.DecodeString(s[1])
	if err != nil {
		return false
	}

	pair := strings.SplitN(string(b), ":", 2)
	if len(pair) != 2 {
		return false
	}

	return pair[0] == configGlobal.HttpAuth && pair[1] == configGlobal.HttpPW
}

func configLoadHandler(rsp http.ResponseWriter, req *http.Request) {
	if checkAuth(req) {
		loadServerList()
		rsp.Write([]byte("success!"))
		return
	}

	rsp.Header().Set("WWW-Authenticate", `Basic realm="MAYDAYX REALM"`)
	rsp.WriteHeader(http.StatusUnauthorized)
	rsp.Write([]byte("401 Unauthorized\n"))
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
