package main

import (
	"flag"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/app"
	"github.com/fpawel/gotools/pkg/ccolor"
	"github.com/fpawel/gotools/pkg/rungo"
	"github.com/powerman/structlog"
	"io"
	"os"
	"path/filepath"
	"strings"
)

func main() {

	defaultLogLevelStr := os.Getenv("MIL82_LOG_LEVEL")
	if len(strings.TrimSpace(defaultLogLevelStr)) == 0 {
		defaultLogLevelStr = "info"
	}

	logLevel := flag.String("log.level", defaultLogLevelStr, "log `level` (debug|info|warn|err)")

	flag.Parse()

	logFileOutput := rungo.NewLogFileOutput()
	defer structlog.DefaultLogger.ErrIfFail(logFileOutput.Close)

	structlog.DefaultLogger.
		//SetLogFormat(structlog.JSON).
		//SetTimeFormat(time.RFC3339Nano).
		//SetTimeValFormat(time.RFC3339Nano).
		// Wrong log.level is not fatal, it will be reported and set to "debug".
		SetOutput(io.MultiWriter(
			ccolor.NewWriter(os.Stderr),
			logFileOutput,
			rungo.NewNotifyGUIWriter(internal.PeerWindowClassName, notify.MsgWriteConsole))).
		SetLogLevel(structlog.ParseLevel(*logLevel)).
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
			structlog.KeyTime:   " %[2]s",
			structlog.KeySource: " %6[2]s",
			structlog.KeyUnit:   " %6[2]s",
			internal.LogKeyWork: " %[1]s=`%[2]s`",
		})
	modbus.SetLogKeysFormat()

	app.Run()
}
