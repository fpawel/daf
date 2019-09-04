package app

import (
	"context"
	"fmt"
	"github.com/ansel1/merry"
	"github.com/fpawel/comm"
	"github.com/fpawel/comm/comport"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/gohelp"
	"github.com/fpawel/gohelp/myfmt"
	"github.com/powerman/structlog"
	"strings"
	"sync"
	"time"
)

type worker struct {
	log                    *structlog.Logger
	ctx                    context.Context
	works                  []string
	portProducts, portHart *comport.ReadWriter
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
			log.ErrIfFail(worker.portProducts.Close)
			log.ErrIfFail(worker.portHart.Close)
			wgWork.Done()
		}()

		notify.WorkStarted(worker.log.Info, workName)
		err := work(worker)
		if err == nil {
			worker.log.Info("выполнено успешно")
			go notify.WorkComplete(worker.log.Info, types.WorkResultInfo{workName, types.WrOk, "успешно"})
			return
		}

		if merry.Is(err, context.Canceled) {
			worker.log.Warn("выполнение прервано")
			go notify.WorkComplete(worker.log.Info, types.WorkResultInfo{workName, types.WrCanceled, "перервано"})
			return
		}
		worker.log.PrintErr(err, "stack", myfmt.FormatMerryStacktrace(err))
		go notify.WorkComplete(worker.log.Info, types.WorkResultInfo{workName, types.WrError, err.Error()})
	}()
}

func newWorker(ctx context.Context, name string) worker {
	return worker{
		log:   gohelp.NewLogWithSuffixKeys("work", fmt.Sprintf("`%s`", name)),
		ctx:   ctx,
		works: []string{name},

		portProducts: comport.NewReadWriter(func() comport.Config {
			return comport.Config{
				Baud:        9600,
				ReadTimeout: time.Millisecond,
				Name:        cfg.GetConfig().ComportProducts,
			}
		}, func() comm.Config {
			return comm.Config{
				ReadByteTimeoutMillis: 50,
				ReadTimeoutMillis:     1000,
				MaxAttemptsRead:       3,
			}
		}),

		portHart: comport.NewReadWriter(func() comport.Config {
			return comport.Config{
				Name:        cfg.GetConfig().ComportHart,
				Baud:        1200,
				ReadTimeout: time.Millisecond,
				Parity:      comport.ParityOdd,
				StopBits:    comport.Stop1,
			}
		}, func() comm.Config {
			return comm.Config{
				ReadByteTimeoutMillis: 50,
				ReadTimeoutMillis:     2000,
				MaxAttemptsRead:       5,
			}
		}),
	}
}

func (x worker) withLogKeys(keyvals ...interface{}) worker {
	x.log = gohelp.LogPrependSuffixKeys(x.log, keyvals...)
	return x
}

func (x worker) performf(format string, args ...interface{}) func(func(x worker) error) error {
	return func(work func(x worker) error) error {
		return x.perform(fmt.Sprintf(format, args...), work)
	}
}

func (x worker) perform(name string, work func(x worker) error) error {
	x.log.Info("выполнить: " + name)
	x.works = append(x.works, name)
	x.log = gohelp.LogPrependSuffixKeys(x.log, fmt.Sprintf("work%d", len(x.works)), fmt.Sprintf("`%s`", name))
	notify.Status(nil, strings.Join(x.works, ": "))
	if err := work(x); err != nil {
		return merry.Append(err, name)
	}
	x.works = x.works[:len(x.works)-1]
	notify.Status(nil, strings.Join(x.works, ": "))
	return nil
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

	notify.Warning(x.log.PrintErr,
		fmt.Sprintf("Не удалось выполнить: %s\n\nПричина: %s", x.works[len(x.works)-1], strErr))
	if merry.Is(x.ctx.Err(), context.Canceled) {
		return err
	}
	x.log.Warn("проигнорирована ошибка: " + err.Error())
	return nil
}
