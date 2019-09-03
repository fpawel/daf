package app

import (
	"context"
	"github.com/fpawel/gohelp/winapp"
	"github.com/fpawel/mil82/internal/api/notify"
	"github.com/fpawel/mil82/internal/data"
	"github.com/fpawel/mil82/internal/dseries"
	"github.com/lxn/win"
	"github.com/powerman/structlog"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
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

	log.Println("charts: updated at", dseries.UpdatedAt())

	var cancel func()
	ctxApp, cancel = context.WithCancel(context.TODO())
	closeHttpServer := startHttpServer()

	if os.Getenv("ELCO_SKIP_RUN_PEER") != "true" {
		if err := exec.Command(filepath.Join(filepath.Dir(os.Args[0]), "mil82gui.exe")).Start(); err != nil {
			panic(err)
		}
	}
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
	log.ErrIfFail(dseries.Close)
}

var (
	ctxApp         context.Context
	cancelWorkFunc = func() {}
	skipDelayFunc  = func() {}
	wgWork         sync.WaitGroup
	log            = structlog.New()
)
