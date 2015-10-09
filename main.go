package main

import (
	"bytes"
	"net/http"
	//"net/url"
	"encoding/json"
	"flag"
	"fmt"
	"log"
	"time"
)

const addr = "127.0.0.1:8000"

var debug = flag.Bool("t", false, "")

type readerServer struct {
	files []string
}

func (rs *readerServer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	err := json.NewEncoder(w).Encode(rs.files)
	if err != nil {
		log.Print(err)
		http.Error(w, "Something Happened", 500)
		return
	}
}

func ReaderServer(a []string) *readerServer {
	return &readerServer{a}
}

type fileHandler struct {
	path string
}

func (s *fileHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	http.ServeFile(w, r, s.path)
}

func load(p string) []byte {
	data, err := Asset(p)
	if err != nil {
		log.Fatal(err)
	}
	return data
}

type bServe struct {
	name string
	data []byte
}

func serveData(p string) http.Handler {
	if *debug {
		return &bServe{p, nil}
	} else {
		return &bServe{p, load(p)}
	}
}

var modTime = time.Now()

func (s *bServe) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	if *debug {
		http.ServeFile(w, r, s.name)
	} else {
		http.ServeContent(w, r, s.name, modTime, bytes.NewReader(s.data))
	}
}

func main() {
	dir := flag.String("d", ".", "serve from this directory")
	flag.Parse()
	args := flag.Args()
	fmt.Println("http://" + addr + "/viewer")
	http.Handle("/files/", http.StripPrefix("/files/", http.FileServer(http.Dir(*dir))))
	http.Handle("/files.json", ReaderServer(args))
	for _, f := range []struct {
		from string
		to   string
	}{
		{"/viewer/", "index.html"},
		{"/elm.js", "elm.js"},
		{"/style.css", "style.css"},
		{"/bootstrap.min.css", "bootstrap.min.css"},
	} {
		http.Handle(f.from, serveData(f.to))
	}
	log.Fatal(http.ListenAndServe(addr, nil))
}
