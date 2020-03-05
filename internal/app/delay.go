package app

import (
	"context"
	"fmt"
	"github.com/ansel1/merry"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/party"
	"github.com/fpawel/daf/internal/pkg"
	"time"
)

func delayf(x worker, duration time.Duration, format string, a ...interface{}) error {
	return delay(x, duration, fmt.Sprintf(format, a...))
}

func delay(x worker, duration time.Duration, name string) error {
	startTime := time.Now()
	x.log = pkg.LogPrependSuffixKeys(x.log, "start", startTime.Format("15:04:05"))
	ctxRootWork := x.ctx

	{
		var skipDelay context.CancelFunc
		x.ctx, skipDelay = context.WithTimeout(x.ctx, duration)
		skipDelayFunc = func() {
			skipDelay()
			x.log.Info("задержка прервана", "elapsed", pkg.FormatDuration(time.Since(startTime)))
		}
	}

	testName := "Фоновый опрос: " + name
	if len(x.works) > 0 {
		testName = x.works[len(x.works)-1] + ": " + testName
	}

	clearTestEntries(testName)

	return x.performf("%s: %s", name, pkg.FormatDuration(duration))(func(x worker) error {
		x.log.Info("задержка начата")
		defer func() {
			notify.EndDelay(x.log.Info, "", "elapsed", pkg.FormatDuration(time.Since(startTime)))
		}()

		mErrors := make(map[int64]struct{})

		for {
			if len(party.CheckedProducts()) == 0 {
				return merry.New("для опроса необходимо установить галочку для как минимум одиного прибора")
			}
			for _, p := range party.CheckedProducts() {
				notify.Delay(nil, types.DelayInfo{
					What:           name,
					TotalSeconds:   int(duration.Seconds()),
					ElapsedSeconds: int(time.Since(startTime).Seconds()),
				})
				_, err := x.readFloatVar(p, varConcentration)
				if err == nil {
					_, err = x.read6408(p)
				}
				if ctxRootWork.Err() != nil {
					return ctxRootWork.Err()
				}
				if x.ctx.Err() != nil {
					return nil // задержка истекла или пропущена пользователем
				}
				if err != nil {
					if _, f := mErrors[p.ProductID]; !f {
						x.log.PrintErr(err)
						addTestEntry(p.ProductID, testName, false, err.Error())
						mErrors[p.ProductID] = struct{}{}
					}
				}
			}
		}
	})
}
