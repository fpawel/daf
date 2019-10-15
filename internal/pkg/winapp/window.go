package winapp

import (
	"github.com/fpawel/daf/internal/pkg/must"
	"github.com/lxn/win"
	"syscall"
	"unsafe"
)

type WindowProcedure = func(hWnd win.HWND, msg uint32, wParam, lParam uintptr) uintptr

func NewWindowWithClassName(windowClassName string, windowProcedure WindowProcedure) win.HWND {

	wndProc := func(hWnd win.HWND, msg uint32, wParam, lParam uintptr) uintptr {
		switch msg {
		case win.WM_DESTROY:
			win.PostQuitMessage(0)
		default:
			return windowProcedure(hWnd, msg, wParam, lParam)
		}
		return 0
	}

	mustRegisterWindowClassWithWndProcPtr(
		windowClassName, syscall.NewCallback(wndProc))

	return win.CreateWindowEx(
		0,
		must.UTF16PtrFromString(windowClassName),
		nil,
		0,
		0,
		0,
		0,
		0,
		win.HWND_TOP,
		0,
		win.GetModuleHandle(nil),
		nil)
}

func mustRegisterWindowClassWithWndProcPtr(className string, wndProcPtr uintptr) {
	mustRegisterWindowClassWithWndProcPtrAndStyle(className, wndProcPtr, 0)
}

func mustRegisterWindowClassWithWndProcPtrAndStyle(className string, wndProcPtr uintptr, style uint32) {

	hInst := win.GetModuleHandle(nil)
	if hInst == 0 {
		panic("GetModuleHandle")
	}

	hIcon := win.LoadIcon(hInst, win.MAKEINTRESOURCE(7)) // rsrc uses 7 for app icon
	if hIcon == 0 {
		hIcon = win.LoadIcon(0, win.MAKEINTRESOURCE(win.IDI_APPLICATION))
	}
	if hIcon == 0 {
		panic("LoadIcon")
	}

	hCursor := win.LoadCursor(0, win.MAKEINTRESOURCE(win.IDC_ARROW))
	if hCursor == 0 {
		panic("LoadCursor")
	}

	var wc win.WNDCLASSEX
	wc.CbSize = uint32(unsafe.Sizeof(wc))
	wc.LpfnWndProc = wndProcPtr
	wc.HInstance = hInst
	wc.HIcon = hIcon
	wc.HCursor = hCursor
	wc.HbrBackground = win.COLOR_BTNFACE + 1
	wc.LpszClassName = must.UTF16PtrFromString(className)
	wc.Style = style

	if atom := win.RegisterClassEx(&wc); atom == 0 {
		panic("RegisterClassEx")
	}
}
