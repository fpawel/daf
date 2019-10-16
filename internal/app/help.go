package app

import (
	"context"
	"github.com/ansel1/merry"
	"github.com/fpawel/comm"
	"github.com/fpawel/daf/internal/pkg"
	"github.com/powerman/structlog"
	"time"
)

func logPrependSuffixKeys(log comm.Logger, a ...interface{}) *structlog.Logger {
	return pkg.LogPrependSuffixKeys(log, a...)
}

func formatBool(b bool, strTrue, strFalse string) string {
	if b {
		return strTrue
	}
	return strFalse
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

//func millis(n int) time.Duration {
//	return time.Duration(n) * time.Millisecond
//}

func minutes(n int) time.Duration {
	return time.Duration(n) * time.Minute
}

func isFailWork(err error) bool {
	if err == nil {
		return false
	}
	if merry.Is(err, context.Canceled) || merry.Is(err, ErrHardware) {
		return true
	}
	if merry.Is(err, comm.Err) || merry.Is(err, context.DeadlineExceeded) {
		return false
	}
	return true
}

func isCommErrorOrDeadline(err error) bool {
	return merry.Is(err, comm.Err) || merry.Is(err, context.DeadlineExceeded)
}
