package app

import (
	"time"
)

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
