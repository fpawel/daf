package app

import (
	"context"
	"github.com/fpawel/daf/internal"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/pkg/winapp"
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

	data.OpenProd()

	log := structlog.New()

	// Завершить процесс, если приложение запущено ранее.
	if winapp.IsWindow(winapp.FindWindow(internal.WindowClassName)) {
		hWnd := winapp.FindWindow(internal.DelphiWindowClassName)
		win.ShowWindow(hWnd, win.SW_RESTORE)
		win.SetForegroundWindow(hWnd)
		log.Fatal("daf.exe already executing")
	}

	if !winapp.IsWindow(winapp.NewWindowWithClassName(internal.WindowClassName, win.DefWindowProc)) {
		panic("window was not created")
	}

	var cancel func()
	ctxApp, cancel = context.WithCancel(context.TODO())
	closeHttpServer := startHttpServer()

	go func() {
		done := make(chan os.Signal, 1)
		signal.Notify(done, syscall.SIGHUP, syscall.SIGINT, syscall.SIGTERM, syscall.SIGQUIT)
		<-done
		log.Info("signal close accepted")
		internal.CloseHWnd()
		return
	}()

	if err := exec.Command(filepath.Join(filepath.Dir(os.Args[0]), "dafgui.exe")).Start(); err != nil {
		panic(err)
	}
	log.Info("run dafgui.exe")

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
	log.ErrIfFail(data.DB.Close)
	cfg.Save()
	log.Debug("all canceled and closed")
}

var (
	ctxApp         context.Context
	cancelWorkFunc = func() {}
	skipDelayFunc  = func() {}
	wgWork         sync.WaitGroup
	//log            = structlog.New()
)
