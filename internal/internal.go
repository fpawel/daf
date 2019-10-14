package internal

import (
	"github.com/fpawel/daf/internal/pkg/winapp"
	"github.com/lxn/win"
	"github.com/powerman/structlog"
)

const (
	LogKeyWork           = "work"
	LogKeyParentWork     = "parent_work"
	LogKeyHardwareDevice = "hardware_device"
	LogProductID         = "product_id"
	LogProductSerial     = "product_serial"
	LogProductPlace      = "product_place"
	LogKeyGasValve       = "gas_valve"
	LogValueGasSwitcher  = "gas_switcher"
	LogKeyDafVar         = "daf_var"
	LogKeyDafCmd         = "daf_cmd"
)

const (
	DelphiWindowClassName = "TMainFormDaf"
	WindowClassName       = "DafServerWindow"
)

func HWnd() win.HWND {
	return winapp.FindWindow(WindowClassName)
}

//func HWndDelphi() win.HWND{
//	return winapp.FindWindow(DelphiWindowClassName)
//}

func CloseHWnd() {
	log.Debug("close window")
	win.PostMessage(HWnd(), win.WM_CLOSE, 0, 0)
	winapp.EnumWindowsWithClassName(func(hWnd win.HWND, winClassName string) {
		if winClassName == DelphiWindowClassName {
			win.PostMessage(hWnd, win.WM_CLOSE, 0, 0)
		}
	})
}

var log = structlog.New()
