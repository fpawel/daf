package pkg

import (
	"bytes"
	"fmt"
	"github.com/ansel1/merry"
	"path/filepath"
	"runtime"
	"strconv"
	"time"
)

func FormatFloat(v float64, prec int) string {
	return strconv.FormatFloat(v, 'f', prec, 64)
}

func FormatDuration(d time.Duration) string {

	d = d.Round(time.Second)

	h := d / time.Hour
	d -= h * time.Hour

	m := d / time.Minute
	d -= m * time.Minute

	s := d / time.Second

	return fmt.Sprintf("%02d:%02d:%02d", h, m, s)
}

// FormatMerryStacktrace returns the error's stacktrace as a string formatted
// the same way as golangs runtime package.
// If e has no stacktrace, returns an empty string.
func FormatMerryStacktrace(e error) string {
	s := merry.Stack(e)
	if len(s) == 0 {
		return ""
	}
	buf := bytes.Buffer{}
	for i, fp := range s {
		fnc := runtime.FuncForPC(fp)
		if fnc != nil {
			f, l := fnc.FileLine(fp)
			name := filepath.Base(fnc.Name())
			ident := " "
			if i > 0 {
				ident = "\t"
			}
			buf.WriteString(fmt.Sprintf("%s%s:%d %s\n", ident, f, l, name))
		}
	}
	return buf.String()

}
