package app

import (
	"context"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/gohelp/winapp"
	"github.com/lxn/win"
	"github.com/powerman/structlog"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"sync"
	"syscall"
)

func Run() {

	// Преверяем, не было ли приложение запущено ранее.
	// Если было, выдвигаем окно UI приложения на передний план и завершаем процесс.
	if notify.ServerWindowAlreadyExists {
		hWnd := winapp.FindWindow(notify.PeerWindowClassName)
		win.ShowWindow(hWnd, win.SW_RESTORE)
		win.SetForegroundWindow(hWnd)
		log.Fatal("daf.exe already executing")
	}

	var cancel func()
	ctxApp, cancel = context.WithCancel(context.TODO())
	closeHttpServer := startHttpServer()

	if os.Getenv("DAF_SKIP_RUN_PEER") != "true" {
		if err := exec.Command(filepath.Join(filepath.Dir(os.Args[0]), "dafgui.exe")).Start(); err != nil {
			panic(err)
		}
	}

	go func() {
		done := make(chan os.Signal, 1)
		signal.Notify(done, syscall.SIGHUP, syscall.SIGINT, syscall.SIGTERM, syscall.SIGQUIT)
		<-done
		log.Info("signal close accepted")
		notify.Window.Close()
		return
	}()

	// цикл оконных сообщений
	for {
		var msg win.MSG
		if win.GetMessage(&msg, 0, 0, 0) == 0 {
			break
		}
		win.TranslateMessage(&msg)
		win.DispatchMessage(&msg)
	}
	cancel()
	closeHttpServer()
	notify.Window.Close()
	log.ErrIfFail(data.DB.Close)
	cfg.Save()
	log.Debug("all canceled and closed")
}

var (
	ctxApp         context.Context
	cancelWorkFunc = func() {}
	skipDelayFunc  = func() {}
	wgWork         sync.WaitGroup
	log            = structlog.New()
)
