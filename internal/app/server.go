package app

import (
	"github.com/fpawel/daf/internal/api"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/gohelp/must"
	"github.com/powerman/rpc-codec/jsonrpc2"
	"github.com/tdewolff/minify"
	"github.com/tdewolff/minify/html"
	"golang.org/x/sys/windows/registry"
	"net"
	"net/http"
	"net/rpc"
)

func startHttpServer() func() {

	for _, svcObj := range []interface{}{
		new(api.PartySvc),
		new(cfg.ConfigSvc),
		api.NewRunnerSvc(runner{}),
		new(api.ProductsSvc),
	} {
		must.AbortIf(rpc.Register(svcObj))
	}

	// Server provide a HTTP transport on /rpc endpoint.
	http.Handle("/rpc", jsonrpc2.HTTPHandler(nil))
	minifyHtml := minify.New()
	minifyHtml.AddFunc("text/html", html.Minify)

	http.Handle("/assets/",
		http.StripPrefix("/assets/",
			http.FileServer(http.Dir("assets"))))

	srv := new(http.Server)
	lnHTTP, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		panic(err)
	}
	addr := "http://" + lnHTTP.Addr().String()
	key, _, err := registry.CreateKey(registry.CURRENT_USER, `daf\http`, registry.ALL_ACCESS)
	if err != nil {
		panic(err)
	}
	if err := key.SetStringValue("addr", addr); err != nil {
		panic(err)
	}
	log.ErrIfFail(key.Close)
	log.Debug(addr)

	go func() {
		err := srv.Serve(lnHTTP)
		if err == http.ErrServerClosed {
			return
		}
		log.PrintErr(err)
		log.ErrIfFail(lnHTTP.Close)
	}()

	return func() {
		if err := srv.Shutdown(ctxApp); err != nil {
			log.PrintErr(err)
		}
	}
}
