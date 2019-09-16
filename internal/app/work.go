package app

import (
	"fmt"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/daf"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/party"
	"github.com/fpawel/gohelp/myfmt"
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
		p := data.LastParty()
		f := func(k float64) float64 {
			return p.ScaleBegin + k*(p.ScaleEnd-p.ScaleBegin)
		}
		if err := x.writeProducts(tnSetupParams, cmdSetupThr1, f(0.2)); err != nil {
			return err
		}
		if err := x.writeProducts(tnSetupParams, cmdSetupThr2, f(0.7)); err != nil {
			return err
		}
		if err := x.writeProducts(tnSetupParams, cmdSetupType, float64(p.ProductType)); err != nil {
			return err
		}
		if err := x.writeProducts(tnSetupParams, cmdSetupTemp, cfg.GetConfig().Temperature); err != nil {
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
			dv, err := x.readDafIndication(p)
			if isFailWork(err) {
				return nil
			}
			v, err := x.read6408(p)
			if err != nil {
				return nil
			}
			x.log.Info(fmt.Sprintf("сохранение для паспорта: %+v, %+v", dv, v))
			data.DB.MustExec(`
REPLACE INTO product_test(product_id, test_number, concentration, output_current, thr1, thr2) 
VALUES (?,?,?,?,?,?)`, p.ProductID, n, dv.C, v.I, v.Thr1, v.Thr1)

			testConcentrationResult{
				C:         dv.C,
				Dc:        pc.CnTest(n) - dv.C,
				I:         v.I,
				Ci:        pc.Ci(v.I),
				Di:        pc.CnTest(n) - pc.Ci(v.I),
				AbsErrLim: pc.AbsErrLimitTest(n),
				OkC:       pc.TestConcentrationOk(n, dv.C),
				OkI:       pc.TestOutputCurrentOk(n, v.I),
				Thr1:      v.Thr1,
				Thr2:      v.Thr2,
				MustThr1:  daf.MustThr1[n],
				MustThr2:  daf.MustThr2[n],
			}.addEntry(p.ProductID, what)
			return nil
		})
	})
}

type testConcentrationResult struct {
	C, Dc, I, Ci, Di, AbsErrLim    float64
	OkC, OkI                       bool
	Thr1, Thr2, MustThr1, MustThr2 bool
}

func (x testConcentrationResult) addEntry(productID int64, what string) {
	f1 := func(v float64) string {
		return myfmt.FormatFloat(v, 1)
	}
	fb := func(v bool) string {
		return formatBool(v, "соответсвует", "не соответствует")
	}
	fbp := func(v bool) string {
		return formatBool(v, "ВКЛ", "выкл.")
	}
	s := fmt.Sprintf(
		`конц.=%v погр.=%v макс.=%v - %s
ток=%v конц.=%v погр.=%v - %s
ПОРОГ1=%q, должно быть %q - %s
ПОРОГ2=%q, должно быть %q - %s`,
		f1(x.C),
		f1(x.Dc),
		f1(x.AbsErrLim),
		fb(x.OkC),
		f1(x.I),
		f1(x.Ci),
		f1(x.Di),
		fb(x.OkI),
		fbp(x.Thr1),
		fbp(x.MustThr1),
		fb(x.Thr1 == x.MustThr1),
		fbp(x.Thr2),
		fbp(x.MustThr2),
		fb(x.Thr2 == x.MustThr2),
	)

	ok := x.OkC && x.OkI && x.Thr1 == x.MustThr1 && x.Thr2 == x.MustThr2

	addTestEntry(productID, what, ok, s)
}
