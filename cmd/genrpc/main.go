package main

import (
	"github.com/fpawel/daf/internal/api"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/pkg/must"
	"os"
	"path/filepath"
	r "reflect"
)

func main() {
	delphiSrcDir := filepath.Join(os.Getenv("GOPATH"), "src", "github.com", "fpawel", "daf", "gui", "api")

	must.EnsureDir(delphiSrcDir)

	servicesSrc := NewServicesUnit("daf", []r.Type{
		r.TypeOf((*api.PartySvc)(nil)),
		r.TypeOf((*cfg.ConfigSvc)(nil)),
		r.TypeOf((*api.RunnerSvc)(nil)),
		r.TypeOf((*api.ProductsSvc)(nil)),
	})
	notifySvcSrc := NewNotifyServicesSrc(servicesSrc.TypesUnit, []NotifyServiceType{
		{
			"Panic",
			r.TypeOf((*string)(nil)).Elem(),
		},
		{
			"PlaceConnection",
			r.TypeOf((*types.PlaceConnection)(nil)).Elem(),
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
			"ProductDataChanged",
			r.TypeOf((*int64)(nil)).Elem(),
		},
		{
			"WriteConsole",
			r.TypeOf((*string)(nil)).Elem(),
		},
	})

	createFile := func(fileName string) *os.File {
		return must.Create(filepath.Join(delphiSrcDir, fileName))
	}

	file := createFile("services.pas")
	servicesSrc.WriteUnit(file)
	must.Close(file)

	file = createFile("server_data_types.pas")
	servicesSrc.TypesUnit.WriteUnit(file)
	must.Close(file)

	file = createFile("notify_services.pas")
	notifySvcSrc.WriteUnit(file)
	must.Close(file)

	file = must.Create(filepath.Join(os.Getenv("GOPATH"),
		"src", "github.com", "fpawel", "daf", "internal", "api", "notify", "api_generated.go"))
	notifySvcSrc.WriteGoFile(file)
	must.Close(file)
}
