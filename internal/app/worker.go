package app

import (
	"context"
	"fmt"
	"github.com/ansel1/merry"
	"github.com/fpawel/comm/comport"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/party"
	"github.com/fpawel/daf/internal/pkg"
	"github.com/powerman/structlog"
	"strings"
	"sync"
	"time"
)

type worker struct {
	log          *structlog.Logger
	ctx          context.Context
	works        []string
	portProducts *comport.Port
	portHart     *comport.Port
	gas          *int
}

func runWork(workName string, work func(x worker) error) {

	cancelWorkFunc()
	wgWork.Wait()
	wgWork = sync.WaitGroup{}
	var ctxWork context.Context
	ctxWork, cancelWorkFunc = context.WithCancel(ctxApp)
	wgWork.Add(1)

	worker := newWorker(ctxWork, workName)

	go func() {
		defer func() {
			worker.log.ErrIfFail(worker.portProducts.Close)
			worker.log.ErrIfFail(worker.portHart.Close)
			wgWork.Done()
		}()

		notify.WorkStarted(worker.log.Info, workName)
		err := work(worker)
		if err == nil {
			worker.log.Info("выполнено успешно")
			notify.WorkComplete(worker.log.Info, types.WorkResultInfo{workName, types.WrOk, "успешно"})
			return
		}

		if merry.Is(err, context.Canceled) {
			worker.log.Warn("выполнение прервано")
			notify.WorkComplete(worker.log.Info, types.WorkResultInfo{workName, types.WrCanceled, "перервано"})
			return
		}
		worker.log.PrintErr(err, "stack", pkg.FormatMerryStacktrace(err))
		notify.WorkComplete(worker.log.Info, types.WorkResultInfo{workName, types.WrError, err.Error()})
	}()
}

func newWorker(ctx context.Context, name string) worker {
	return worker{
		gas:   new(int),
		log:   pkg.NewLogWithSuffixKeys("work", fmt.Sprintf("%s", name)),
		ctx:   ctx,
		works: []string{name},

		portProducts: comport.NewPort(func() comport.Config {
			return comport.Config{
				Baud:        9600,
				ReadTimeout: time.Millisecond,
				Name:        cfg.GetConfig().ComportProducts,
			}
		}),
		portHart: comport.NewPort(func() comport.Config {
			return comport.Config{
				Name:        cfg.GetConfig().ComportHart,
				Baud:        1200,
				ReadTimeout: time.Millisecond,
				Parity:      comport.ParityOdd,
				StopBits:    comport.Stop1,
			}
		}),
	}
}

func (x worker) ReaderDaf() modbus.ResponseReader {
	return x.portProducts.NewResponseReader(x.ctx, cfg.GetConfig().Comm.Daf)
}
func (x worker) Reader6408() modbus.ResponseReader {
	return x.portProducts.NewResponseReader(x.ctx, cfg.GetConfig().Comm.EN6408)
}
func (x worker) ReaderGas() modbus.ResponseReader {
	return x.portProducts.NewResponseReader(x.ctx, cfg.GetConfig().Comm.Gas)
}
func (x worker) ReaderHart() modbus.ResponseReader {
	return x.portHart.NewResponseReader(x.ctx, cfg.GetConfig().Comm.Hart)
}

func (x worker) pause(t time.Duration) {
	_ = x.performf("пауза %v", t)(func(x worker) error {
		pause(x.ctx.Done(), t)
		return nil
	})
}

func (x worker) performf(format string, args ...interface{}) func(func(x worker) error) error {
	return func(work func(x worker) error) error {
		return x.perform(fmt.Sprintf(format, args...), work)
	}
}

func (x worker) perform(name string, work func(x worker) error) error {
	x.log = logPrependSuffixKeys(structlog.New(),
		internal.LogKeyWork, name,
		internal.LogKeyParentWork, fmt.Sprintf("%q", x.works),
	)
	x.works = append(x.works, name)
	notify.Status(nil, strings.Join(x.works, ": "))
	if err := work(x); err != nil {
		return merry.Append(err, name)
	}
	x.works = x.works[:len(x.works)-1]
	notify.Status(nil, strings.Join(x.works, ": "))
	return nil
}

func (x worker) performTest(name string, work func(x worker) error) error {
	return x.perform(name, func(x worker) error {
		clearTestEntries(name)
		return work(x)
	})
}

func (x worker) performWithWarn(work func() error) error {
	err := work()
	if err == nil {
		return nil
	}
	if merry.Is(x.ctx.Err(), context.Canceled) {
		return err
	}
	return x.raiseWarning(err)
}

func (x worker) raiseWarning(err error) error {
	strErr := strings.Join(strings.Split(err.Error(), ": "), "\n\t -")

	notify.Warning(nil,
		fmt.Sprintf("Не удалось выполнить: %s\n\nПричина: %s", x.works[len(x.works)-1], strErr))
	if merry.Is(x.ctx.Err(), context.Canceled) {
		return err
	}
	x.log.PrintErr(merry.Append(err, "ошибка проигнорирована пользователем"))
	return nil
}

func (x worker) writeProducts(testName string, cmd dafCmd, arg float64) error {
	return x.performf("%s код=%d параметр=%v", cmd.Name, cmd.Code, arg)(func(x worker) error {
		return x.performProducts(testName, func(p party.Product, x worker) error {
			return x.writeProduct(p, cmd, arg)
		})
	})
}

func (x worker) performProducts(testName string, work func(p party.Product, x worker) error) error {
	products := party.CheckedProducts()
	if len(products) == 0 {
		return errNoCheckedProducts.Here()
	}
	for _, p := range products {
		x.log = pkg.LogPrependSuffixKeys(x.log,
			internal.LogProductSerial, p.Serial,
			internal.LogProductID, p.ProductID,
			internal.LogProductPlace, p.Place)

		err := work(p, x)
		if err != nil {
			x.log.PrintErr(err, structlog.KeyStack, structlog.Auto)
			addTestEntry(p.ProductID, testName, false, err.Error())
		}
		if isFailWork(err) {
			return err
		}
	}
	return nil
}

func clearTestEntries(testName string) {

	data.DB.MustExec(`
DELETE FROM product_entry 
WHERE test = ? AND product_id IN (
    SELECT product_id 
    FROM product    
    WHERE party_id = (
        SELECT party_id
		FROM party
		ORDER BY created_at DESC
		LIMIT 1) )`, testName)
	for _, p := range party.Products() {
		notify.ProductDataChanged(nil, p.ProductID)
	}
}

func addTestEntry(productID int64, testName string, ok bool, result string) {
	data.DB.MustExec(`INSERT INTO product_entry(product_id, test, ok, message) VALUES (?, ?, ?, ?)`,
		productID, testName, ok, result)
	notify.ProductDataChanged(nil, productID)
}
