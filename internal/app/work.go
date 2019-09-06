package app

import (
	"fmt"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/party"
	"github.com/fpawel/gohelp"
	"time"
)

type testWork string

const (
	testWorkSoftVersion = "Проверка версии ПО"
	testSetupCurrent    = "Настройка токового выхода"
)

func (x worker) testSoftVersion() error {

	c := cfg.GetConfig()
	return x.performTest(testWorkSoftVersion, func(p party.Product) (bool, string, error) {

		softVer, err := x.dafReadFloat(p, varSoftVer, "", nil)
		if err != nil {
			return false, "", err
		}
		softVerID, err := x.dafReadFloat(p, varSoftVerID, "", nil)
		if err != nil {
			return false, "", err
		}

		ok := byte(softVer) == c.SoftVersion && uint16(softVerID) == c.SoftVersionID
		text := fmt.Sprintf("%d ID %X", int(softVer), uint16(softVerID))
		if !ok {
			text += fmt.Sprintf(": должно быть %d ID %X",
				int(c.SoftVersion), uint16(c.SoftVersionID),
			)
		}
		return ok, text, nil
	})
}

func (x worker) setupCurrent() error {

	x.log.Info("настройка токового выхода")
	if err := x.writeProducts(testSetupCurrent, 0xB, 1); err != nil {
		return err
	}
	pause(x.ctx.Done(), 5*time.Second)
	x.log.Info("корректировка тока 4 мА")

	current4 := make(map[int64]float64)
	if err := x.doForEachCheckedProduct(testSetupCurrent, func(p party.Product) error {
		v, err := x.read6408(p) // считать ток со стенда
		if err != nil {
			return err
		}
		current4[p.ProductID] = v.Current
		return x.writeProduct(p, 9, v.Current)
	}); err != nil {
		return err
	}

	if err := x.writeProducts(testSetupCurrent, 0xC, 1); err != nil {
		return err
	}

	pause(x.ctx.Done(), 5*time.Second)

	x.log.Info("корректировка тока 20 мА")
	var current20 float64
	if err := x.doForEachCheckedProduct(testSetupCurrent, func(p party.Product) error {
		v, err := x.read6408(p)
		if err != nil {
			return err
		}
		current20 = v.Current
		return x.writeProduct(p, 0xA, v.Current)
	}); err != nil {
		return err
	}
	setTestResult(p)

	return nil
	return true, fmt.Sprintf("%v мА, %v мА"), nil

	return x.performTest(testSetupCurrent, func(p party.Product) (bool, string, error) {

	})

}

func (x worker) writeProducts(testName testWork, cmd modbus.DevCmd, arg float64) error {
	if len(testName) > 0 {
		notify.Status(nil, fmt.Sprintf("%s: отправка команды %X, %v", testName, cmd, arg))
	} else {
		notify.Status(nil, fmt.Sprintf("отправка команды %X, %v", cmd, arg))
	}

	req := modbus.NewWrite32BCDRequest(0, 0x10, cmd, arg)
	if cmd == 5 {
		_, err := x.portProducts.Write(x.log, x.ctx, req.Bytes())
		return err
	}
	return doForEachCheckedProduct(testName, func(p party.Product) error {
		return x.writeProduct(p, cmd, arg)
	})
}

func clearTestData(testName testWork) {
	data.DB.MustExec(`
DELETE FROM product_work 
WHERE work = ? AND product_id IN (
    SELECT product_id 
    FROM product    
    WHERE party_id = (
        SELECT party_id
		FROM party
		ORDER BY created_at DESC
		LIMIT 1) )`, testName)
}

func setTestResult(p party.Product, testName testWork, ok bool, result string) {
	data.DB.MustExec(`
REPLACE INTO product_work(product_id, work, stored_at, ok, message) 
VALUES (?, ?, ?, ?, ?)`, p.ProductID, testName, time.Now(), ok, result)
}

func (x worker) doForEachCheckedProduct(testName testWork, work func(p party.Product) error) error {
	if len(testName) > 0 {
		x.log = gohelp.LogAppendPrefixKeys(x.log, "test", testName)
	}
	products := party.CheckedProducts()
	if len(products) == 0 {
		return errNoCheckedProducts.Here()
	}
	for _, p := range products {
		err := work(p)
		if err != nil && len(testName) > 0 {
			setTestResult(p, testName, false, err.Error())
		}
		if isFailWork(err) {
			return err
		}
	}
	return nil
}
