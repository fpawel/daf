package app

import (
	"fmt"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/party"
	"time"
)

const (
	tnSoftVersion    = "Проверка версии ПО"
	tnSetupCurrent   = "Настройка токового выхода"
	tnThresholdsTest = "Установка порогов для настройки"
	tnAdjust         = "Калибровка"
)

func (x worker) testSoftVersion() error {
	return x.performTest(tnSoftVersion, func(x worker) error {
		return performProducts(tnSoftVersion, func(p party.Product) error {
			c := cfg.GetConfig()
			softVer, err := x.readFloat(p, varSoftVer, "", nil)
			if err != nil {
				return err
			}
			softVerID, err := x.readFloat(p, varSoftVerID, "", nil)
			if err != nil {
				return err
			}
			ok := byte(softVer) == c.SoftVersion && uint16(softVerID) == c.SoftVersionID
			text := fmt.Sprintf("%d ID %X", int(softVer), uint16(softVerID))
			if !ok {
				text += fmt.Sprintf(": должно быть %d ID %X",
					int(c.SoftVersion), uint16(c.SoftVersionID),
				)
			}
			addTestEntry(p.ProductID, tnSoftVersion, ok, text)
			return nil
		})
	})
}

func (x worker) setupCurrent() error {
	return x.performTest(tnSetupCurrent, func(x worker) error {
		if err := x.writeProducts(tnSetupCurrent, 0xB, 1); err != nil {
			return err
		}
		pause(x.ctx.Done(), 5*time.Second)
		x.log.Info("корректировка тока 4 мА")
		if err := performProducts(tnSetupCurrent, func(p party.Product) error {
			v, err := x.read6408(p) // считать ток со стенда
			if err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupCurrent, true, fmt.Sprintf("4 мА: %v мА", v.OutputCurrent))
			return x.writeProduct(p, 9, v.OutputCurrent)
		}); err != nil {
			return err
		}
		if err := x.writeProducts(tnSetupCurrent, 0xC, 1); err != nil {
			return err
		}
		pause(x.ctx.Done(), 5*time.Second)
		if err := performProducts(tnSetupCurrent, func(p party.Product) error {
			v, err := x.read6408(p)
			if err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupCurrent, true, fmt.Sprintf("20 мА: %v мА", v.OutputCurrent))
			return x.writeProduct(p, 0xA, v.OutputCurrent)
		}); err != nil {
			return err
		}
		return nil
	})

}

func (x worker) setupThresholdTest() error {
	return x.performTest(tnThresholdsTest, func(x worker) error {
		c := data.LastParty()
		if err := x.writeProducts(tnThresholdsTest, 0x30, c.Thr1Test); err != nil {
			return err
		}
		return x.writeProducts(tnThresholdsTest, 0x31, c.Thr2Test)
	})
}

func (x worker) adjust() error {
	return x.performTest(tnAdjust, func(x worker) error {
		c := data.LastParty()
		x.log.Info("корректировка нулевых показаний")
		defer func() {
			_ = x.switchGas(0)
		}()
		if err := x.blowGas(1); err != nil {
			return err
		}
		if err := x.writeProducts(tnAdjust, 0x32, c.C1); err != nil {
			return err
		}
		x.log.Info("корректировка чувствительности")
		if err := x.blowGas(4); err != nil {
			return err
		}
		if err := x.writeProducts(tnAdjust, 0x33, c.C4); err != nil {
			return err
		}
		return nil
	})
}

func (x worker) testMeasure() error {
	defer func() {
		_ = x.switchGas(0)
	}()
	for n, gas := range []int{1, 2, 3, 4, 3, 1} {
		if err := x.testMeasureAt(n, gas); err != nil {
			return err
		}
	}
	return nil
}

func (x worker) testMeasureAt(n, gas int) error {
	what := fmt.Sprintf("проверка диапазона измерений: №%d, ПГС%d", n, gas)
	return x.performTest(what, func(x worker) error {
		if err := x.blowGas(gas); err != nil {
			return err
		}
		data.DB.MustExec(`
DELETE FROM product_test 
WHERE test_number = ? AND product_id IN ( SELECT product_id FROM last_party_products) `, n)
		return performProducts(what, func(p party.Product) error {
			x := x
			dv, err := x.readDafIndication(p)
			if isFailWork(err) {
				return nil
			}
			v, err := x.read6408(p)
			if err != nil {
				return nil
			}
			p.WrapLog(x.log).Info(fmt.Sprintf("сохранение для паспорта: %+v, %+v", dv, v))
			data.DB.MustExec(`
REPLACE INTO product_test(product_id, test_number, concentration, output_current, thr1, thr2) 
VALUES (?,?,?,?,?,?)`, p.ProductID, n, dv.Concentration, v.OutputCurrent, v.Threshold1, v.Threshold2)
			addTestEntry(p.ProductID, what, true, "выполнено")
			return nil
		})
	})
}
