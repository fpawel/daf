package report

import (
	"github.com/fpawel/daf/internal/data"
	"math"
	"strconv"
	"strings"
	"time"
)

type Table = []Row

type Row = []Cell

type Cell struct {
	Text      string
	Alignment Alignment
	Color     string
}

type Alignment int

const (
	taLeftJustify Alignment = iota
	taRightJustify
	taCenter
)

func header(xs []string) (r Row) {
	for _, x := range xs {
		r = append(r, Cell{
			Text:      x,
			Alignment: taCenter,
		})
	}
	return
}

func floatCellC(v float64, prec int, ok bool) Cell {
	c := Cell{
		Alignment: taRightJustify,
		Text:      formatFloat(v, prec),
		Color:     "clBlue",
	}
	if !ok {
		c.Color = "clRed"
	}
	return c
}

func IndividualPassport1(productID int64) Table {

	row6 := func(s string) []Cell {
		xs := make([]Cell, 7)
		xs[0] = Cell{Text: s, Alignment: taLeftJustify}
		return xs
	}
	r := []Row{
		header([]string{
			"Параметр", "1. ПГС1", "2. ПГС2", "3. ПГС3", "4. ПГС4", "5. ПГС3", "6. ПГС1",
		}),
		row6("конц."),
		row6("ток, мА"),
		row6("конц.(ток)"),
		row6("порог 1"),
		row6("порог 2"),
		row6("Вариация"),
	}
	p := data.GetPartyByProductID(productID)

	var values []struct {
		Test       int     `db:"test_number"`
		C          float64 `db:"concentration"`
		I          float64 `db:"output_current"`
		T1         bool    `db:"thr1"`
		T2         bool    `db:"thr2"`
		ScaleBegin float64 `db:"scale_begin"`
		ScaleEnd   float64 `db:"scale_end"`
	}

	if err := data.DB.Select(&values, `
SELECT test_number, concentration, output_current, thr1 ,thr2, scale_begin, scale_end
FROM product_test 
INNER JOIN product USING (product_id)
WHERE product_id = ? 
ORDER BY test_number`, productID); err != nil {
		panic(err)
	}

	var (
		C3i   float64
		C3set bool
	)

	tests := p.Tests()

	for _, x := range values {
		test := tests[x.Test]

		dC := x.C - test.Nominal

		okC := math.Abs(dC) < test.AbsErrorLimit

		Ci := data.CurrentToConcentration(x.I, x.ScaleBegin, x.ScaleEnd)
		dCi := Ci - test.Nominal
		okCi := math.Abs(dCi) < test.AbsErrorLimit

		if x.Test == 2 {
			C3set = true
			C3i = Ci
		}

		r[1][x.Test+1] = floatCellC(x.C, 3, okC)
		r[2][x.Test+1] = floatCellC(x.I, 3, okCi)
		r[3][x.Test+1] = floatCellC(Ci, 3, okCi)
		r[4][x.Test+1] = onOffCell(x.T1, x.T1 == test.MustThr1)
		r[5][x.Test+1] = onOffCell(x.T2, x.T2 == test.MustThr2)
		if x.Test == 4 && C3set && p.C3 != 0 {
			variation := C3i - Ci
			r[6][x.Test+1] = floatCellC(variation, 1, math.Abs(variation) < p.VariationLimit3)
		}
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
		header([]string{
			"Время", "Работа", "Результат",
		}),
	}
	for _, x := range xs {
		c := Cell{
			Text:      x.Message,
			Alignment: taLeftJustify,
			Color:     "clNavy",
		}
		if !x.Ok {
			c.Color = "clRed"
		}
		r = append(r, Row{
			Cell{
				Text:      x.StoredAt.In(time.Local).Format("2006-01-02 15:04"),
				Alignment: taCenter,
			},
			Cell{
				Text:      x.Test,
				Alignment: taLeftJustify,
			},
			c,
		})
	}
	return r

}

func formatFloat(v float64, prec int) string {
	s := strconv.FormatFloat(v, 'f', prec, 64)
	return strings.TrimRight(strings.TrimRight(s, "0"), ".")
}

func onOffCell(v, ok bool) Cell {
	c := Cell{
		Text:      "ВКЛ",
		Alignment: taCenter,
		Color:     "clBlue",
	}
	if !v {
		c.Text = "выкл"
	}
	if !ok {
		c.Color = "clRed"
	}
	return c
}
