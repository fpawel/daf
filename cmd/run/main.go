package main

import (
	"flag"
	"github.com/fpawel/daf/internal"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/gotools/pkg/rungo"
	"github.com/powerman/structlog"
	"os"
	"path/filepath"
)

func main() {
	args := flag.String("args", "", "command line arguments to pass")
	flag.Parse()
	rungo.Cmd{
		ExeName: "daf.exe",
		ExeArgs: *args,
		UseGUI:  true,
		NotifyGUI: rungo.NotifyGUI{
			MsgCodeConsole: notify.MsgWriteConsole,
			MsgCodePanic:   notify.MsgPanic,
			WindowClass:    internal.PeerWindowClassName,
		},
	}.Exec()
}

func init() {
	structlog.DefaultLogger.
		SetPrefixKeys(
			structlog.KeyApp, structlog.KeyPID, structlog.KeyLevel, structlog.KeyUnit, structlog.KeyTime,
		).
		SetDefaultKeyvals(
			structlog.KeyApp, filepath.Base(os.Args[0]),
			structlog.KeySource, structlog.Auto,
		).
		SetSuffixKeys(
			structlog.KeyStack,
		).
		SetSuffixKeys(structlog.KeySource).
		SetKeysFormat(map[string]string{
			structlog.KeySource: " %6[2]s",
			structlog.KeyUnit:   " %6[2]s",
		})
}
