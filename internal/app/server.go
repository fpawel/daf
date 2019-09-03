package app

import (
	"fmt"
	"github.com/fpawel/gohelp/must"
	"github.com/fpawel/mil82/internal/api"
	"github.com/fpawel/mil82/internal/data"
	"github.com/fpawel/mil82/internal/dseries"
	"github.com/fpawel/mil82/internal/mil82/reporthtml"
	"github.com/powerman/rpc-codec/jsonrpc2"
	"github.com/tdewolff/minify"
	"github.com/tdewolff/minify/html"
	"golang.org/x/sys/windows/registry"
	"net"
	"net/http"
	"net/rpc"
	"strconv"
)

func startHttpServer() func() {

	for _, svcObj := range []interface{}{
		new(api.LastPartySvc),
		new(api.ConfigSvc),
		api.NewRunnerSvc(runner{}),
		new(dseries.ChartsSvc),
		new(api.PartiesSvc),
	} {
		must.AbortIf(rpc.Register(svcObj))
	}

	// Server provide a HTTP transport on /rpc endpoint.
	http.Handle("/rpc", jsonrpc2.HTTPHandler(nil))

	http.HandleFunc("/chart", dseries.HandleRequestChart)

	minifyHtml := minify.New()
	minifyHtml.AddFunc("text/html", html.Minify)

	http.HandleFunc("/report", func(w http.ResponseWriter, r *http.Request) {
		mw := minifyHtml.ResponseWriter(w, r)
		defer log.ErrIfFail(mw.Close)
		w = mw

		w.WriteHeader(200)
		w.Header().Set("Content-Type", "text/html")
		w.Header().Set("Accept", "text/html")

		s := r.URL.Query().Get("party_id")
		var partyID int64
		if s == "last" {
			partyID = data.LastParty().PartyID
		} else {
			partyID, _ = strconv.ParseInt(s, 10, 64)
		}
		reporthtml.WriteViewParty(w, partyID)
	})

	http.Handle("/assets/",
		http.StripPrefix("/assets/",
			http.FileServer(http.Dir("assets"))))

	srv := new(http.Server)
	lnHTTP, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		panic(err)
	}
	addr := "http://" + lnHTTP.Addr().String()
	fmt.Printf("%s/report?party_id=last\n", addr)
	key, _, err := registry.CreateKey(registry.CURRENT_USER, `daf\http`, registry.ALL_ACCESS)
	if err != nil {
		panic(err)
	}
	if err := key.SetStringValue("addr", addr); err != nil {
		panic(err)
	}
	log.ErrIfFail(key.Close)

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
