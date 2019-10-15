package app

import (
	"fmt"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/party"
	"strconv"
	"time"
)

const (
	tnSoftVersion  = "Проверка версии ПО"
	tnSetupCurrent = "Настройка токового выхода"
	tnSetupParams  = "Установка параметров"
	tnAdjust       = "Калибровка"
)

func (x worker) testSoftVersion() error {
	return x.performTest(tnSoftVersion, func(x worker) error {
		return x.performProducts(tnSoftVersion, func(p party.Product, x worker) error {
			c := cfg.GetConfig()
			softVer, err := x.readFloat(p, varSoftVer, "")
			if err != nil {
				return err
			}
			softVerID, err := x.readFloat(p, varSoftVerID, "")
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
		if err := x.writeProducts(tnSetupCurrent, cmdMode4, 1); err != nil {
			return err
		}

		x.log.Info("корректировка тока 4 мА")
		if err := x.performProducts(tnSetupCurrent, func(p party.Product, x worker) error {
			v, err := x.read6408(p) // считать ток со стенда
			if err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupCurrent, true, fmt.Sprintf("4 мА: %v мА", v.I))
			return x.writeProduct(p, cmdSetup4, v.I)
		}); err != nil {
			return err
		}

		x.pause(10 * time.Second)

		if err := x.writeProducts(tnSetupCurrent, cmdMode20, 1); err != nil {
			return err
		}

		if err := x.performProducts(tnSetupCurrent, func(p party.Product, x worker) error {
			v, err := x.read6408(p)
			if err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupCurrent, true, fmt.Sprintf("20 мА: %v мА", v.I))
			return x.writeProduct(p, cmdSetup20, v.I)
			//return x.writeProduct(p, 0xA, 20)
		}); err != nil {
			return err
		}
		return nil
	})

}

func (x worker) setupParams() error {
	return x.performTest(tnSetupParams, func(x worker) error {
		lastParty := data.LastParty()
		productType := float64(lastParty.ProductType)
		gas := float64(lastParty.Component)
		temp := cfg.GetConfig().Temperature

		if err := x.performProducts(tnSetupParams, func(p party.Product, x worker) error {
			if err := x.writeProduct(p, cmdSetupType, productType); err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupParams, true,
				fmt.Sprintf("установлено исполнение %v", productType))

			if err := x.writeProduct(p, cmdSetupComponent, gas); err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupParams, true,
				fmt.Sprintf("установлен газ %v", gas))

			if err := x.writeProduct(p, cmdSetupTemp, temp); err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupParams, true,
				fmt.Sprintf("установлена температура %v\"C", temp))

			scaleEnd, err := x.readDafFloat(p, varMeasureRange)
			if err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupParams, true,
				fmt.Sprintf("считан диапазон измерения %v", scaleEnd))

			data.MustSetProductScaleBegin0(p.ProductID)
			data.MustSetProductScaleEnd(p.ProductID, scaleEnd)

			thr1, thr2 := 0.2*scaleEnd, 0.7*scaleEnd

			if err := x.writeProduct(p, cmdSetupThr1, thr1); err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupParams, true,
				fmt.Sprintf("установлен порог1 %v", thr1))

			if err := x.writeProduct(p, cmdSetupThr2, thr2); err != nil {
				return err
			}
			addTestEntry(p.ProductID, tnSetupParams, true,
				fmt.Sprintf("установлен порог2 %v", thr2))

			return nil

		}); err != nil {
			return err
		}

		return nil
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
		if err := x.writeProducts(tnAdjust, cmdAdjustBeg, c.C1); err != nil {
			return err
		}
		x.log.Info("корректировка чувствительности")
		if err := x.blowGas(4); err != nil {
			return err
		}
		if err := x.writeProducts(tnAdjust, cmdAdjustEnd, c.C4); err != nil {
			return err
		}
		return nil
	})
}

func (x worker) testMeasure() error {
	for n, gas := range []int{1, 2, 3, 4, 3, 1} {
		if err := x.testMeasureAt(n, gas); err != nil {
			return err
		}
	}
	return nil
}

func (x worker) testMeasureAt(n, gas int) error {
	what := fmt.Sprintf("проверка показаний №%d ПГС%d", n+1, gas)
	return x.performTest(what, func(x worker) error {
		if err := x.blowGas(gas); err != nil {
			return err
		}
		pc := data.LastParty()
		data.DB.MustExec(`
DELETE FROM product_test 
WHERE test_number = ? AND product_id IN ( SELECT product_id FROM last_party_products) `, n)
		return x.performProducts(what, func(p party.Product, x worker) error {
			concentration, err := x.readDafFloat(p, varConcentration)
			if isFailWork(err) {
				return nil
			}
			scaleEnd, err := x.readDafFloat(p, varMeasureRange)
			if err != nil {
				return nil
			}
			value6408, err := x.read6408(p)
			if err != nil {
				return nil
			}
			x.log.Info(fmt.Sprintf("сохранение для паспорта: %+v, %+v", concentration, value6408))

			data.MustSetProductScaleBegin0(p.ProductID)
			data.MustSetProductScaleEnd(p.ProductID, scaleEnd)

			data.DB.MustExec(`
REPLACE INTO product_test(product_id, test_number, concentration, output_current, thr1, thr2) 
VALUES (?,?,?,?,?,?)`, p.ProductID, n, concentration, value6408.I, value6408.Thr1, value6408.Thr1)

			test := pc.Tests()[n]

			testResult{
				Test:          test,
				Concentration: concentration,
				Current:       value6408.I,
				ScaleEnd:      scaleEnd,
				Thr1:          value6408.Thr1,
				Thr2:          value6408.Thr2,
			}.addEntry(p.ProductID, what)
			return nil
		})
	})
}

type testResult struct {
	data.Test
	Concentration,
	Current,
	ScaleEnd float64
	Thr1, Thr2 bool
}

func (x testResult) AbsErrorConcentration() float64 {
	return x.Nominal - x.Concentration
}
func (x testResult) AbsErrorCurrent() float64 {
	return x.Nominal - data.CurrentToConcentration(x.Current, 0, x.ScaleEnd)
}
func (x testResult) ConcentrationOk() bool {
	return x.Test.ConcentrationOk(x.Concentration)
}
func (x testResult) CurrentOk() bool {
	return x.Test.CurrentOk(x.Current, 0, x.ScaleEnd)
}

func (x testResult) addEntry(productID int64, what string) {
	f1 := func(v float64) string {
		return strconv.FormatFloat(v, 'g', -1, 64)
	}
	fb := func(v bool) string {
		return formatBool(v, "соответсвует", "не соответствует")
	}
	fbp := func(v bool) string {
		return formatBool(v, "ВКЛ", "выкл.")
	}

	concentrationCurrent := data.CurrentToConcentration(x.Current, 0, x.ScaleEnd)

	s := fmt.Sprintf(
		`конц.=%v погр.=%v макс.=%v - %s
диапазон 0...%v,
ток=%v конц.=%v погр.=%v - %s
ПОРОГ1=%q, должно быть %q - %s
ПОРОГ2=%q, должно быть %q - %s`,
		f1(x.Concentration),
		f1(x.Nominal-x.Concentration),
		f1(x.AbsErrorLimit),
		fb(x.ConcentrationOk()),
		f1(x.ScaleEnd),
		f1(x.Current),
		f1(concentrationCurrent),
		f1(x.Nominal-concentrationCurrent),
		fb(x.CurrentOk()),
		fbp(x.Thr1),
		fbp(x.MustThr1),
		fb(x.Thr1 == x.MustThr1),
		fbp(x.Thr2),
		fbp(x.MustThr2),
		fb(x.Thr2 == x.MustThr2),
	)

	ok := x.ConcentrationOk() && x.CurrentOk() && x.Thr1 == x.MustThr1 && x.Thr2 == x.MustThr2

	addTestEntry(productID, what, ok, s)
}
