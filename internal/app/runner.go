package app

import (
	"fmt"
	"github.com/ansel1/merry"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/party"
	"github.com/powerman/structlog"
)

type runner struct{}

func (_ runner) SetNetAddress(addr modbus.Addr) {
	runWork(fmt.Sprintf("Установка адреса %d", addr), func(x worker) error {
		return x.setNetAddress(addr)
	})
}

func (_ runner) SwitchGas(n int) {
	runWork(fmt.Sprintf("Газовый блок %d", n), func(x worker) error {
		return x.switchGas(n)
	})
}

func (_ runner) Cancel() {
	cancelWorkFunc()
	log := structlog.New()
	log.Info("выполнение прервано")
}

func (_ runner) SkipDelay() {
	skipDelayFunc()
	log := structlog.New()
	log.Info("задержка прервана")
}

func (_ runner) Read3(Var modbus.Var, bcd bool) {
	column := fmt.Sprintf("%d (%X)", Var, Var)
	runWork(fmt.Sprintf("read3 %d", Var), func(x worker) error {
		for _, p := range party.CheckedProducts() {
			if bcd {
				_, _ = x.readFloat(p, Var, column)
			} else {
				_, _ = x.read2(p, Var, column)
			}

		}
		return nil
	})
}

func (_ runner) Write32(cmd modbus.DevCmd, value float64) {
	runWork(fmt.Sprintf("write32 %d %v", cmd, value), func(x worker) error {
		for _, p := range party.CheckedProducts() {
			_ = x.writeProduct(p, dafCmd{Code: cmd}, value)
		}
		return nil
	})
}

func (_ runner) RunMainWork(c []bool) {
	runWork("Настройка ДАФ-М", func(x worker) error {

		defer func() {
			_ = x.switchGas(0)
		}()

		for i, fun := range []func() error{
			x.testSoftVersion,
			x.setupParams,
			x.setupCurrent,
			x.adjust,
			x.testMeasure,
			x.testMeasure2,
		} {
			if i >= len(c) {
				x.log.Warn(fmt.Sprintf("работа %d: индекс должен быть от 0 до %d", i, len(c)-1))
				continue
			}
			if !c[i] {
				x.log.Warn(fmt.Sprintf("работа %d: галочка снята", i))
				continue
			}
			if err := fun(); err != nil {
				return err
			}
		}
		return nil
	})
}

func (_ runner) RunReadVars() {

	runWork("опрос", func(x worker) error {
		for {
			products := party.CheckedProducts()
			if len(products) == 0 {
				return errNoCheckedProducts.Here()
			}
			for _, p := range products {
				if err := x.interrogate(p); err != nil {
					return err
				}
			}
		}
	})
}

var (
	errNoCheckedProducts = merry.New("для опроса необходимо установить галочку для как минимум одного прибора")
)
