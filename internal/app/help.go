package app

import (
	"context"
	"github.com/ansel1/merry"
	"github.com/fpawel/comm"
	"github.com/fpawel/daf/internal/party"
	"github.com/fpawel/gohelp"
	"github.com/powerman/structlog"
	"time"
)

func formatOnOf(b bool) string {
	if b {
		return "ВКЛ"
	}
	return "выкл"
}

func pause(chDone <-chan struct{}, d time.Duration) {
	timer := time.NewTimer(d)
	for {
		select {
		case <-timer.C:
			return
		case <-chDone:
			timer.Stop()
			return
		}
	}
}

func millis(n int) time.Duration {
	return time.Duration(n) * time.Millisecond
}

func minutes(n int) time.Duration {
	return time.Duration(n) * time.Minute
}

func isFailWork(err error) bool {
	return err != nil && !isDeviceError(err)
}

func isDeviceError(err error) bool {
	return merry.Is(err, comm.Err) || merry.Is(err, context.DeadlineExceeded)
}

func logProduct(log *structlog.Logger, p party.Product) *structlog.Logger {
	return gohelp.LogPrependSuffixKeys(log,
		"place", p.Place,
		"addr", p.Addr,
		"product_id", p.ProductID,
		"serial", p.Serial)
}
