package internal

import (
	"os"
	"path/filepath"
)

func DataDir() string {
	if os.Getenv("DAF_DEV_DB") == "true" {
		return filepath.Join(os.Getenv("GOPATH"), "src", "github.com", "fpawel", "daf", "build")
	}
	return filepath.Dir(os.Args[0])
}

const (
	PeerWindowClassName   = "TMainFormDaf"
	ServerWindowClassName = "DafServerWindow"
	EnvKeySkipRunPeer     = "DAF_SKIP_RUN_PEER"
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
	LogKeyDeviceVar      = "device_var"
)
