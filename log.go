package main

import (
	"database/sql"
	"fmt"
	"log"
	"time"
)

type logStruct struct {
	channel  string
	serverId string
	orderId  string
	appId    string
	ext      string
}

var logChan chan logStruct

const dbConnectionCount = 2

func init() {
	logChan = make(chan logStruct, dbConnectionCount)
}

func (logItem logStruct) write() {
	logChan <- logItem
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
