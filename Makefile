ms: data.go main.go
	go build -o $@

data.go: elm.js index.html style.css bootstrap.min.css
	go-bindata -nomemcopy -o $@ $^

elm.js: Viewer.elm Main.elm
	elm-package install -y
	elm-make --output elm.js Main.elm
