package main

import (
	"encoding/xml"
	"github.com/natefinch/lumberjack"
	"io/ioutil"
	"log"
	"net/http"
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
	http.HandleFunc(configGlobal.ServerListLoadUrl, configLoadHandler)
	http.HandleFunc("/proxy_v2_recharge", hjRecharge)
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
