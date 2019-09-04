package main

import (
	"github.com/fpawel/daf/internal/api"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/gohelp/delphi/delphirpc"
	"os"
	"path/filepath"
	r "reflect"
)

func main() {

	delphirpc.WriteSources(delphirpc.SrcServices{
		Name: "daf",
		Dir:  filepath.Join(os.Getenv("GOPATH"), "src", "github.com", "fpawel", "daf", "gui", "api"),
		Types: []r.Type{
			r.TypeOf((*api.LastPartySvc)(nil)),
			r.TypeOf((*cfg.ConfigSvc)(nil)),
			r.TypeOf((*api.RunnerSvc)(nil)),
			r.TypeOf((*api.PartiesSvc)(nil)),
		},
	}, delphirpc.SrcNotify{
		PeerWindowClassName:   "TMainFormDaf",
		ServerWindowClassName: "DafServerWindow",
		Dir: filepath.Join(os.Getenv("GOPATH"),
			"src", "github.com", "fpawel", "daf", "internal", "api", "notify"),
		Types: []delphirpc.NotifyServiceType{
			{
				"Panic",
				r.TypeOf((*string)(nil)).Elem(),
			},
			{
				"ReadProductValue",
				r.TypeOf((*types.ProductValue)(nil)).Elem(),
			},
			{
				"ProductError",
				r.TypeOf((*types.ProductError)(nil)).Elem(),
			},
			{
				"WorkStarted",
				r.TypeOf((*string)(nil)).Elem(),
			},
			{
				"WorkComplete",
				r.TypeOf((*types.WorkResultInfo)(nil)).Elem(),
			},
			{
				"Warning",
				r.TypeOf((*string)(nil)).Elem(),
			},
			{
				"Delay",
				r.TypeOf((*types.DelayInfo)(nil)).Elem(),
			},
			{
				"EndDelay",
				r.TypeOf((*string)(nil)).Elem(),
			},
			{
				"Status",
				r.TypeOf((*string)(nil)).Elem(),
			},
		},
	})

}
