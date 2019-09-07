package app

import (
	"github.com/ansel1/merry"
	"github.com/fpawel/daf/internal/party"
	"github.com/powerman/structlog"
)

type runner struct{}

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

func (_ runner) RunMainWork() {
	runWork("Настройка ДАФ-М", func(x worker) error {
		for _, fun := range []func() error{
			x.testSoftVersion,
			x.setupCurrent,
			x.setupThresholdTest,
			x.testMeasure,
		} {
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
				if _, _, err := x.readProduct(p); err != nil && !isDeviceError(err) {
					return err
				}
			}
		}
	})
}

var errNoCheckedProducts = merry.New("для опроса необходимо установить галочку для как минимум одного прибора")
