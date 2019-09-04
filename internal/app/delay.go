package app

import (
	"context"
	"fmt"
	"github.com/ansel1/merry"
	"github.com/fpawel/daf/internal/api/notify"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/party"
	"github.com/fpawel/gohelp"
	"github.com/fpawel/gohelp/myfmt"
	"time"
)

func delayf(x worker, duration time.Duration, format string, a ...interface{}) error {
	return delay(x, duration, fmt.Sprintf(format, a...))
}

func delay(x worker, duration time.Duration, name string) error {
	startTime := time.Now()
	x.log = gohelp.LogPrependSuffixKeys(x.log, "start", startTime.Format("15:04:05"))

	{
		var skipDelay context.CancelFunc
		x.ctx, skipDelay = context.WithTimeout(x.ctx, duration)
		skipDelayFunc = func() {
			skipDelay()
			x.log.Info("задержка прервана", "elapsed", myfmt.FormatDuration(time.Since(startTime)))
		}
	}
	ctxRootWork := x.ctx

	return x.performf("%s: %s", name, myfmt.FormatDuration(duration))(func(x worker) error {
		x.log.Info("задержка начата")
		defer func() {
			go notify.EndDelay(x.log.Info, "elapsed", myfmt.FormatDuration(time.Since(startTime)))
		}()
		for {
			if len(party.CheckedProducts()) == 0 {
				return merry.New("для опроса необходимо установить галочку для как минимум одиного прибора")
			}
			for _, p := range party.CheckedProducts() {
				_, _, err := x.readPlace(p)
				if ctxRootWork.Err() != nil {
					return ctxRootWork.Err()
				}
				if x.ctx.Err() != nil {
					return nil // задержка истекла или пропущена пользователем
				}
				if err != nil {
					if err = x.raiseWarning(merry.Append(err, "фоновый опрос")); err != nil {
						return err
					}
				}
				go notify.Delay(nil, types.DelayInfo{
					What:           name,
					TotalSeconds:   int(duration.Seconds()),
					ElapsedSeconds: int(time.Since(startTime).Seconds()),
				})
				pause(x.ctx.Done(), millis(cfg.GetConfig().PauseReadPlaceMillis))
			}
		}
	})
}
