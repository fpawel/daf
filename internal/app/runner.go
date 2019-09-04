package app

import (
	"github.com/ansel1/merry"
	"github.com/fpawel/daf/internal/party"
)

type runner struct{}

func (_ runner) Cancel() {
	cancelWorkFunc()
	log.Info("выполнение прервано")
}

func (_ runner) SkipDelay() {
	skipDelayFunc()
	log.Info("задержка прервана")
}

func (_ runner) RunMainWork() {
	runWork("настройка", func(x worker) error {

		if len(party.CheckedProducts()) == 0 {
			return errNoCheckedProducts.Here()
		}

		//dseries.CreateNewBucket("настройка МИЛ-82")
		//notify.NewChart(x.log.Info)
		//defer dseries.Save()

		if err := x.blowGas(1); err != nil {
			return err
		}
		if err := x.blowGas(2); err != nil {
			return err
		}
		return nil
	})
}

func (_ runner) RunReadVars() {

	runWork("опрос", func(x worker) error {
		if len(party.CheckedProducts()) == 0 {
			return errNoCheckedProducts.Here()
		}
		for {
			if len(party.CheckedProducts()) == 0 {
				return errNoCheckedProducts.Here()
			}
			for _, p := range party.CheckedProducts() {
				_, _, err := x.readPlace(p)
				if err != nil || !isDeviceError(err) {
					return err
				}
			}
		}
	})
}

var errNoCheckedProducts = merry.New("для опроса необходимо установить галочку для как минимум одного прибора")
