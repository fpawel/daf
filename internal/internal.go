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
	LogKeyWork           = "work"
	LogKeyParentWork     = "parent_work"
	LogKeyTest           = "test"
	LogKeyHardwareDevice = "hardware_device"
	LogProductID         = "product_id"
	LogProductSerial     = "product_serial"
	LogProductPlace      = "product_place"
	LogKeyGasValve       = "gas_valve"
	LogValueGasSwitcher  = "gas_switcher"
	LogKeyDeviceVar      = "device_var"
)
