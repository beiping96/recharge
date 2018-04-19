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

const ()

type ZbConfig struct {
	DbIp        string `xml:"db_ip"`
	DbPort      string `xml:"db_port"`
	DbAcc       string `xml:"db_acc"`
	DbPwd       string `xml:"db_pwd"`
	DbName      string `xml:"db_name"`
	ZoneId      int    `xml:"zone_id"`
	RefreshTime string `xml:"refresh_time"`
	ListenAddr  string `xml:"listen_addr"`
	HttpsAddr   string `xml:"https_addr"`
	CertPath    string `xml:"cert_file"`
	KeyPath     string `xml:"key_file"`
	LogPath     string `xml:"log_path"`
}

type ServerCtx struct {
	conn_str string

	// 写入的db连接
	wdb *sql.DB

	// 读取的db连接
	lock sync.RWMutex
	rdb  *sql.DB

	// 当前大区
	zone_id int

	// 当前公告
	content string

	// ip白名单
	ipt         map[string]int
	ipt_enabled bool
}

type ReadRsp struct {
	Result  int    `json:"result"`
	Content string `json:"data"`
	Error   string `json:"error"`
}

type WriteRsp struct {
	Result int    `json:"result"`
	Error  string `json:"error"`
}

var ctx *ServerCtx

func main() {
	if len(os.Args) < 2 {
		print_help()
		return
	}

	cont, err := ioutil.ReadFile("config.xml")
	if err != nil {
		log.Fatal(err)
	}

	var cfg ZbConfig
	err = xml.Unmarshal(cont, &cfg)
	if err != nil {
		log.Fatal(err)
	}

	if cfg.LogPath == "" {
		cfg.LogPath = "./notice.log"
	}
	log.Printf("log path is %s\n", cfg.LogPath)

	switch os.Args[1] {
	case "start":
		// redirect the log system
		log.SetOutput(&lumberjack.Logger{
			Filename:   cfg.LogPath,
			MaxSize:    20,
			MaxBackups: 9,
			MaxAge:     28,
			Compress:   false,
		})

		log.Println(time.Now().Unix())
		log.Printf("notice start for zone %v\n", cfg.ZoneId)

		interval, err := strconv.ParseInt(cfg.RefreshTime, 10, 32)
		if err != nil {
			log.Fatalf("start failed, %s\n", err.Error())
		}

		conn_str := fmt.Sprintf("%s:%s@tcp(%s:%s)/%s?charset=utf8", cfg.DbAcc, cfg.DbPwd, cfg.DbIp, cfg.DbPort, cfg.DbName)

		// init the context
		ctx = &ServerCtx{conn_str: conn_str, rdb: nil, wdb: nil, zone_id: cfg.ZoneId, content: "", ipt_enabled: false}

		// init the db connection
		log.Printf("db connection string: %s\n", conn_str)
		db_init(conn_str)

		// load content from db immediately
		cont, err := load_notice_from_db()
		if err != nil {
			log.Fatalln(err)
		}
		update_notice(cont)

		// start the timer
		go init_load_from_db_timer(int(interval))
		log.Printf("start notice timer for loading from db...\n")

		// start the https service
		if cfg.HttpsAddr != "" {
			go start_https_service(cfg)
		}

		// start the http service
		http.HandleFunc("/", http_read_root)
		http.HandleFunc("/read_notice.py", http_read_notice)
		http.HandleFunc("/write_notice.py", http_write_notice_from_web)
		http.HandleFunc("/write_notice_gmt.py", http_write_notice_from_gmt)
		server := &http.Server{Addr: cfg.ListenAddr, Handler: nil}
		err = server.ListenAndServe()
		if err != nil {
			log.Fatal("ListenAndServe: ", err)
		}
		log.Printf("start notice server success, listen addr: %s\n", cfg.ListenAddr)
		log.Printf("notice start\n")
	case "rwrite":
		rip := os.Args[2]
		remote_write(rip)
	case "rread":
		rip := os.Args[2]
		remote_read(rip)
	case "write":
		remote_write("127.0.0.1")
	case "read":
		remote_read("127.0.0.1")
	default:
		print_help()
	}
}

func print_help() {
	log.Printf("notice help:\n")
	log.Printf("	notice start\n")
	log.Printf("	notice write\n")
	log.Printf("	notice read\n")
	log.Printf("	notice rwrite remote_ip remote_port\n")
	log.Printf("	notice rread remote_ip remote_port\n")
}

func db_init(conn_str string) {
	// read connection
	rdb, err := sql.Open("mysql", conn_str)
	if err != nil {
		log.Fatalln(err)
	}
	err = rdb.Ping()
	if err != nil {
		log.Fatalln(err)
	}
	ctx.rdb = rdb

	// write connection
	wdb, err := sql.Open("mysql", conn_str)
	if err != nil {
		log.Fatalln(err)
	}
	err = wdb.Ping()
	if err != nil {
		log.Fatalln(err)
	}
	ctx.wdb = wdb
}

func load_ip_from_db() (map[string]int, error) {
	ip_map := make(map[string]int, 100)
	sql_str := fmt.Sprintf("select ip from iptable limit 100")
	log.Printf("load ip: %v\n", sql_str)

	rows, err := ctx.rdb.Query(sql_str)
	if err != nil {
		return nil, err
	}
	defer rows.Close()

	for rows.Next() {
		var ip string
		err = rows.Scan(&ip)
		if err != nil {
			continue
		}
		ip_map[ip] = 1
	}

	return ip_map, nil
}

func load_notice_from_db() (string, error) {
	sql_str := fmt.Sprintf("select zone_cont from content where zone_id=%d", ctx.zone_id)
	log.Printf("load notice: %v\n", sql_str)
	rows, err := ctx.rdb.Query(sql_str)
	if err != nil {
		return "", err
	}
	defer rows.Close()

	for rows.Next() {
		var cont string
		err = rows.Scan(&cont)
		if err != nil {
			return "", err
		} else {
			return cont, nil
		}
	}

	return "", nil
}

func save_notice_to_db(content string) error {
	sql_str1 := fmt.Sprintf("delete from content where zone_id=%v", ctx.zone_id)
	sql_str2 := fmt.Sprintf("insert into content (zone_id, zone_cont) values (%v, '%v')", ctx.zone_id, content)
	log.Printf("save notice1: %v\n", sql_str1)
	log.Printf("save notice2: %v\n", sql_str2)

	err := ctx.wdb.Ping()
	if err != nil {
		log.Printf("save error: %v\n", err)
		return err
	}

	trx, _ := ctx.wdb.Begin()
	trx.Exec(sql_str1)
	trx.Exec(sql_str2)
	trx.Commit()

	return nil
}

func update_notice(content string) {
	ctx.lock.Lock()
	defer ctx.lock.Unlock()
	ctx.content = content
}

func update_ipt(ipt map[string]int) {
	ctx.lock.Lock()
	defer ctx.lock.Unlock()
	ctx.ipt = ipt
	if len(ipt) > 0 {
		ctx.ipt_enabled = true
	} else {
		ctx.ipt_enabled = false
	}
}

func ipt_enabled() bool {
	ctx.lock.RLock()
	defer ctx.lock.RUnlock()
	return ctx.ipt_enabled
}

func ipt_contain(rip string) bool {
	ctx.lock.RLock()
	defer ctx.lock.RUnlock()
	_, ok := ctx.ipt[rip]
	return ok
}

func get_notice() string {
	ctx.lock.RLock()
	defer ctx.lock.RUnlock()
	return ctx.content
}

func init_load_from_db_timer(interval int) {
	tick := time.NewTicker(time.Duration(interval) * time.Second)
	for {
		select {
		case <-tick.C:
			// timer is active
			log.Printf("timer is active\n")

			// load the ip table
			ip_map, err := load_ip_from_db()
			if err != nil {
				log.Printf("load ip from db error: %v\n", err)
			} else {
				update_ipt(ip_map)
				log.Printf("ipt enable: %t\n", ipt_enabled())
			}

			// load the notice
			cont, err := load_notice_from_db()
			if err != nil {
				log.Printf("load from db error: %v\n", err)
				continue
			}

			log.Printf("load from db ok, content: %v\n", cont)

			// update the content
			update_notice(cont)
			log.Printf("update notice from db success...\n")
		}
	}
}

func http_read_root(rsp http.ResponseWriter, req *http.Request) {
}

func http_read_notice(rsp http.ResponseWriter, req *http.Request) {
	rip, _, _ := net.SplitHostPort(req.RemoteAddr)
	log.Printf("req from ip: %v method: %v\n", rip, req.Method)

	var result ReadRsp

	result.Result = 0
	result.Content = get_notice()
	result.Error = ""

	body, _ := json.Marshal(result)
	rsp.Header().Add("Access-Control-Allow-Origin", "*")
	rsp.Write(body)
}

func http_write_notice_from_web(rsp http.ResponseWriter, req *http.Request) {
	rip, _, _ := net.SplitHostPort(req.RemoteAddr)
	log.Printf("req from ip: %v method: %v\n", rip, req.Method)

	var result WriteRsp

	if ipt_enabled() && !ipt_contain(rip) {
		log.Printf("error ip address %v\n", rip)
		result.Result = 1
		result.Error = "should in ip white-list"
		body, _ := json.Marshal(result)
		rsp.Header().Add("Access-Control-Allow-Origin", "*")
		rsp.Write(body)
		return
	}

	if req.Method != "POST" {
		log.Printf("error Method %v\n", req.Method)
		result.Result = 1
		result.Error = "should use POST method"
		body, _ := json.Marshal(result)
		rsp.Header().Add("Access-Control-Allow-Origin", "*")
		rsp.Write(body)
		return
	}

	req.ParseForm()
	log.Printf("form: %v\n", req.Form)

	cont := req.FormValue("content")
	if cont == "" {
		log.Printf("error, content is empty")
		result.Result = 1
		result.Error = "should contain 'content' field or not empty"
		body, _ := json.Marshal(result)
		rsp.Header().Add("Access-Control-Allow-Origin", "*")
		rsp.Write(body)
		return
	}

	log.Printf("content is %v\n", cont)
	s := url.QueryEscape(cont)
	log.Printf("content encode %v\n", s)
	save_notice_to_db(s)

	result.Result = 0
	result.Error = ""
	body, _ := json.Marshal(result)
	rsp.Header().Add("Access-Control-Allow-Origin", "*")
	rsp.Write(body)
}

func http_write_notice_from_gmt(rsp http.ResponseWriter, req *http.Request) {
	rip, _, _ := net.SplitHostPort(req.RemoteAddr)
	log.Printf("req from ip: %v method: %v\n", rip, req.Method)

	var result WriteRsp

	if ipt_enabled() && !ipt_contain(rip) {
		log.Printf("error ip address %v\n", rip)
		result.Result = 1
		result.Error = "should in ip white-list"
		body, _ := json.Marshal(result)
		rsp.Header().Add("Access-Control-Allow-Origin", "*")
		rsp.Write(body)
		return
	}

	if req.Method != "POST" {
		log.Printf("error Method %v\n", req.Method)
		result.Result = 1
		result.Error = "should use POST method"
		body, _ := json.Marshal(result)
		rsp.Header().Add("Access-Control-Allow-Origin", "*")
		rsp.Write(body)
		return
	}

	req.ParseForm()
	log.Printf("form: %v\n", req.Form)

	cont := req.FormValue("content")
	if cont == "" {
		log.Printf("error, content is empty")
		result.Result = 1
		result.Error = "should contain 'content' field or not empty"
		body, _ := json.Marshal(result)
		rsp.Header().Add("Access-Control-Allow-Origin", "*")
		rsp.Write(body)
		return
	}

	log.Printf("content is %v\n", cont)
	s := url.QueryEscape(cont)
	log.Printf("content encode %v\n", s)
	save_notice_to_db(s)

	result.Result = 0
	result.Error = ""
	body, _ := json.Marshal(result)
	rsp.Header().Add("Access-Control-Allow-Origin", "*")
	rsp.Write(body)
}

func remote_read(rip string) {
	log.Printf("remote ip %v\n", rip)
	url := fmt.Sprintf("http://%s:14875/read_notice.py", rip)
	log.Printf("url: %v\n", url)

	rsp, err := http.Get(url)
	if err != nil {
		log.Fatalln(err)
	}

	body, err := ioutil.ReadAll(rsp.Body)
	if err != nil {
		log.Fatalln(err)
	}
	rsp.Body.Close()

	message := string(body)
	log.Printf("message is %v\n", message)
}

func remote_write(rip string) {
	log.Printf("remote ip %v\n", rip)
	url := fmt.Sprintf("http://%s:14875/write_notice_gmt.py", rip)
	log.Printf("url: %v\n", url)

	cont := "content=abc"
	rsp, err := http.Post(url, "application/x-www-form-urlencoded", strings.NewReader(cont))
	if err != nil {
		log.Fatalln(err)
	}

	body, err := ioutil.ReadAll(rsp.Body)
	if err != nil {
		log.Fatalln(err)
	}
	rsp.Body.Close()

	message := string(body)
	log.Printf("message is %v\n", message)
}

func start_https_service(config ZbConfig) {
	mux := http.NewServeMux()
	mux.HandleFunc("/", http_read_root)
	mux.HandleFunc("/read_notice.py", http_read_notice)
	err := http.ListenAndServeTLS(config.HttpsAddr, config.CertPath, config.KeyPath, mux)
	if err != nil {
		log.Fatal("ListenAndServeTLS: ", err)
	}
	log.Printf("notice https start\n")
}
