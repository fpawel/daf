package app

import (
	"github.com/ansel1/merry"
	"github.com/fpawel/comm"
	"github.com/fpawel/mil82/internal/api/notify"
	"github.com/fpawel/mil82/internal/cfg"
	"github.com/fpawel/mil82/internal/dseries"
	"github.com/fpawel/mil82/internal/last_party"
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

		if len(last_party.CheckedProducts()) == 0 {
			return errNoCheckedProducts.Here()
		}

		dseries.CreateNewBucket("настройка МИЛ-82")
		notify.NewChart(x.log.Info)

		defer dseries.Save()

		if err := blowGas(x, 1); err != nil {
			return err
		}
		if err := blowGas(x, 2); err != nil {
			return err
		}
		return nil
	})
}

func (_ runner) RunReadVars() {

	runWork("опрос", func(x worker) error {
		if len(last_party.CheckedProducts()) == 0 {
			return errNoCheckedProducts.Here()
		}
		vars := cfg.Get().Vars
		dseries.CreateNewBucket("опрос МИЛ-82")
		notify.NewChart(x.log.Info)
		defer dseries.Save()
		for {
			products := last_party.CheckedProducts()
			if len(products) == 0 {
				return errNoCheckedProducts.Here()
			}
		loopProducts:
			for _, p := range products {
				for _, v := range vars {
					_, err := readProductVar(x, p.Addr, v.Code)
					if err != nil {
						if merry.Is(err, comm.Err) {
							continue loopProducts
						}
						return err
					}
				}
			}
		}
	})
}

var errNoCheckedProducts = merry.New("для опроса необходимо установить галочку для как минимум одного прибора")
