package main

import (
	"flag"
	"fmt"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/app"
	"github.com/fpawel/gotools/pkg/ccolor"
	"github.com/powerman/structlog"
	"io"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

func main() {

	defaultLogLevelStr := os.Getenv("MIL82_LOG_LEVEL")
	if len(strings.TrimSpace(defaultLogLevelStr)) == 0 {
		defaultLogLevelStr = "info"
	}

	logLevel := flag.String("log.level", defaultLogLevelStr, "log `level` (debug|info|warn|err)")
	enableLogFile := flag.Bool("log.file", true, "enable text log to file (true|false)")

	flag.Parse()

	logWriters := []io.Writer{ccolor.NewWriter(os.Stderr), notifyWriter{}}

	if *enableLogFile {
		logFile := newLogFileOutput()
		defer structlog.DefaultLogger.ErrIfFail(logFile.Close)
		logWriters = append(logWriters, logFile)
	}

	structlog.DefaultLogger.
		SetOutput(io.MultiWriter(logWriters...)).
		//SetLogFormat(structlog.JSON).
		//SetTimeFormat(time.RFC3339Nano).
		//SetTimeValFormat(time.RFC3339Nano).
		// Wrong log.level is not fatal, it will be reported and set to "debug".
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

type notifyWriter struct{}

func (x notifyWriter) Write(b []byte) (int, error) {
	go notify.WriteConsole(nil, string(b))
	return len(b), nil
}

func newLogFileOutput() io.WriteCloser {
	exeDir := filepath.Dir(os.Args[0])
	t := time.Now()
	logDir := filepath.Join(exeDir, "logs")
	_, err := os.Stat(logDir)
	if os.IsNotExist(err) {
		err = os.MkdirAll(logDir, os.ModePerm)
	}
	if err != nil {
		panic(err)
	}
	logFileName := filepath.Join(logDir, fmt.Sprintf("%s.log", t.Format("2006-01-02")))
	logFile, err := os.OpenFile(logFileName, os.O_CREATE|os.O_APPEND, 0666)
	if err != nil {
		panic(err)
	}
	return &logFileOutput{logFile, sync.Mutex{}}
}

type logFileOutput struct {
	*os.File
	sync.Mutex
}

func (x *logFileOutput) Write(p []byte) (int, error) {
	go func() {
		x.Lock()
		defer x.Unlock()
		if _, err := fmt.Fprint(x.File, time.Now().Format("15:04:05"), " "); err != nil {
			panic(err)
		}
		if _, err := x.File.Write(p); err != nil {
			panic(err)
		}
	}()
	return len(p), nil
}
