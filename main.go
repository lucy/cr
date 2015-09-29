package main

import (
	"net/http"
	//"net/url"
	"encoding/json"
	"log"
	"flag"
	"fmt"
	"github.com/rs/cors"
)

const addr = "127.0.0.1:8000"

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

func (s fileHandler) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	http.ServeFile(w, r, s.path)
}

func main() {
	flag.Parse()
	args := flag.Args()
	dir := "."
	c := cors.New(cors.Options{
		AllowedOrigins: []string{"*"},
		AllowCredentials: true,
	})
	fmt.Println("http://" + addr + "/viewer")
	http.Handle("/files/", c.Handler(http.StripPrefix("/files/", http.FileServer(http.Dir(dir)))))
	http.Handle("/files.json", c.Handler(ReaderServer(args)))
	http.Handle("/viewer/", fileHandler{"index.html"})
	http.Handle("/elm.js", fileHandler{"elm.js"})
	http.Handle("/style.css", fileHandler{"style.css"})
	http.Handle("/bootstrap.min.css", fileHandler{"bootstrap.min.css"})
	log.Fatal(http.ListenAndServe(addr, nil))
}
