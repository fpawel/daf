package report

import (
	"github.com/fpawel/daf/internal/data"
	"strconv"
	"strings"
	"time"
)

type Table = []Row

type Row = []Cell

type Cell = string

func IndividualPassport1(productID int64) Table {

	row7 := func(s string) []string {
		xs := make([]string, 8)
		xs[0] = s
		return xs
	}
	r := []Row{
		{
			"Параметр", "1. ПГС1", "2. ПГС2", "3. ПГС3", "4. ПГС2", "5. ПГС1", "6. ПГС3", "7. ПГС1",
		},
		row7("концентрация"),
		row7("ток"),
		row7("порог 1"),
		row7("порог 2"),
	}

	var values []struct {
		Test int     `db:"test_number"`
		C    float64 `db:"concentration"`
		I    float64 `db:"output_current"`
		T1   bool    `db:"thr1"`
		T2   bool    `db:"thr2"`
	}

	if err := data.DB.Select(&values, `
SELECT  test_number, concentration, output_current, thr1 ,thr2
FROM product_test 
WHERE product_id = ? 
ORDER BY test_number`, productID); err != nil {
		panic(err)
	}
	for _, x := range values {
		r[1][x.Test+1] = formatFloat(x.C, 3)
		r[2][x.Test+1] = formatFloat(x.I, 3)
		r[3][x.Test+1] = formatOnOff(x.T1)
		r[4][x.Test+1] = formatOnOff(x.T2)
	}
	return r
}

func IndividualPassport2(productID int64) Table {
	var xs []struct {
		Test     string    `db:"test"`
		StoredAt time.Time `db:"stored_at"`
		Message  string    `db:"message"`
		Ok       bool      `db:"ok"`
	}
	if err := data.DB.Select(&xs, `
SELECT test, stored_at, ok, message 
FROM product_entry 
WHERE product_id = ?
ORDER BY stored_at`, productID); err != nil {
		panic(err)
	}
	r := []Row{
		{
			"Время", "Работа", "Результат",
		},
	}
	for _, x := range xs {
		if !x.Ok {
			x.Message += ";clRed"
		} else {
			x.Message += ";clNavy"
		}
		r = append(r, []string{
			x.StoredAt.In(time.Local).Format("15:04"),
			x.Test,
			x.Message,
		})
	}
	return r

}

func formatFloat(v float64, prec int) string {
	s := strconv.FormatFloat(v, 'f', prec, 64)
	return strings.TrimRight(strings.TrimRight(s, "0"), ".")
}

func formatOnOff(v bool) string {
	if v {
		return "ВКЛ"
	}
	return "выкл"
}
