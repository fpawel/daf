package main

import (
	"github.com/fpawel/gohelp/delphi/delphirpc"
	"github.com/fpawel/mil82/internal/api"
	"github.com/fpawel/mil82/internal/api/types"
	"github.com/fpawel/mil82/internal/dseries"
	"os"
	"path/filepath"
	r "reflect"
)

func main() {

	delphirpc.WriteSources(delphirpc.SrcServices{
		Dir: filepath.Join(os.Getenv("DELPHIPATH"),
			"src", "github.com", "fpawel", "mil82gui", "api"),
		Types: []r.Type{
			r.TypeOf((*api.LastPartySvc)(nil)),
			r.TypeOf((*api.ConfigSvc)(nil)),
			r.TypeOf((*api.RunnerSvc)(nil)),
			r.TypeOf((*dseries.ChartsSvc)(nil)),
			r.TypeOf((*api.PartiesSvc)(nil)),
		},
	}, delphirpc.SrcNotify{
		PeerWindowClassName:   "TMainFormMil82",
		ServerWindowClassName: "Mil82ServerWindow",
		Dir: filepath.Join(os.Getenv("GOPATH"),
			"src", "github.com", "fpawel", "daf", "internal", "api", "notify"),
		Types: []delphirpc.NotifyServiceType{
			{
				"Panic",
				r.TypeOf((*string)(nil)).Elem(),
			},
			{
				"ReadVar",
				r.TypeOf((*types.AddrVarValue)(nil)).Elem(),
			},
			{
				"AddrError",
				r.TypeOf((*types.AddrError)(nil)).Elem(),
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
			{
				"NewChart",
				r.TypeOf((*struct{})(nil)).Elem(),
			},
		},
	})

}
