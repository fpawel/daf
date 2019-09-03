package daf

import (
	"database/sql"
	"fmt"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/mil82/internal/data"
	"math"
	"strconv"
	"strings"
)

type Table struct {
	Rows []Row
}

type Row struct {
	Cells []Cell
}

type Cell struct {
	ValueType    ValueType
	Text, Detail string
}

type ValueType int

const (
	vtNone ValueType = iota
	vtOk
	vtError
)

func (x Party) Report(Var modbus.Var) (r Table) {

	r = Table{
		[]Row{
			row1("ID"),
			row1("Сер.№"),
			row1("Адрес"),
		},
	}
	addRows := func(rows []Row) {
		if len(rows) > 1 {
			r.Rows = append(r.Rows, rows...)
		}
	}

	addCell := func(n int, fmtStr string, v interface{}) {
		r.Rows[n].Cells = append(r.Rows[n].Cells, cell1f(fmtStr, v))
	}
	for _, p := range x.Products {
		addCell(0, "%d", p.ProductID)
		addCell(1, "%d", p.Serial)
		addCell(2, "%02d", p.Addr)
	}

	addRows(x.rowsError20())

	for _, dn := range dataTables {

		rows := []Row{row1(dn.title)}

		for _, gas := range dn.gases {
			row := Row{Cells: make([]Cell, len(x.Products)+1)}
			row.Cells[0] = cell1f("ПГС%d", gas)
			hasValue := false
			for i, p := range x.Products {
				v, f := getProductValue(p.ProductID, dn.work, dn.temp, Var, gas)
				if !f {
					continue
				}
				hasValue = true
				row.Cells[i+1] = cell1(strconv.FormatFloat(v, 'f', -1, 64))
			}
			if hasValue {
				rows = append(rows, row)
			}
		}

		addRows(rows)
	}
	return
}

func (x Party) rowsError20() []Row {
	rows := []Row{row1("Погрешность +20⁰С, НКУ, % от допуска")}
	for _, gas := range gases1234 {
		row := Row{Cells: make([]Cell, len(x.Products)+1)}
		row.Cells[0] = cell1f("ПГС%d", gas)
		hasValue := false
		for i, p := range x.Products {
			c, f := getProductValue(p.ProductID, WorkCheckup, Temp20, VarConc, gas)
			if !f {
				continue
			}
			hasValue = true
			n := x.concentration(gas)
			d := c - n
			m := x.ProductType.maxErr20(n)
			vt := vtOk
			if math.Abs(d) >= m {
				vt = vtError
			}
			row.Cells[i+1] = Cell{
				ValueType: vt,
				Text:      formatFloat(d*100./m, 1),
				Detail: fmt.Sprintf(`%s - измеренная концентрация
ПГС%d=%v - номинальная концентрация
%s - абсолютная погрешность
%s - предел абсолютной погрешности`,
					formatFloat(c, 3),
					gas, n,
					formatFloat(d, 3),
					formatFloat(m, 3)),
			}
		}
		if hasValue {
			rows = append(rows, row)
		}
	}
	return rows
}

func getProductValue(productID int64, work Work, temp Temp, Var modbus.Var, gas Gas) (float64, bool) {
	var v float64
	err := data.DB.Get(&v,
		`SELECT value FROM product_value WHERE product_id = ? AND work=? AND temp=? AND var=? AND gas=?`,
		productID, work, temp, Var, gas)
	if err == sql.ErrNoRows {
		return 0, false
	}
	if err != nil {
		panic(err)
	}
	return v, true
}

func formatFloat(v float64, prec int) string {
	s := strconv.FormatFloat(v, 'f', prec, 64)
	return strings.TrimRight(strings.TrimRight(s, "0"), ".")
}

type dataTable struct {
	title string
	work  Work
	temp  Temp
	gases []Gas
}

var (
	gases1234  = []Gas{1, 2, 3, 4}
	gases134   = []Gas{1, 3, 4}
	dataTables = func() []dataTable {
		return []dataTable{
			{"Линеаризация", WorkLin, Temp20, gases1234},
			{"+20⁰С, НКУ, ", WorkTemp, Temp20, gases1234},
			{"«-»⁰С, пониженная температура", WorkTemp, TempMinus, gases134},
			{"«+»⁰С, повышенная температура", WorkTemp, TempPlus, gases134},
			{"+90⁰С", WorkTemp, Temp90, gases134},
			{"Проверка +20⁰С, НКУ", WorkCheckup, Temp20, gases1234},
			{"Проверка «-»⁰С, пониженная температура", WorkCheckup, TempMinus, gases134},
			{"Проверка «+»⁰С, повышенная температура", WorkCheckup, TempPlus, gases134},
			{"1. Первый техпрогон", WorkTex1, Temp20, gases134},
			{"2. Второй техпрогон", WorkTex2, Temp20, gases134},
		}
	}()
)

func cell1(s string) Cell {
	return Cell{Text: s}
}

func cell1f(format string, a ...interface{}) Cell {
	return cell1(fmt.Sprintf(format, a...))
}

func row1(s string) Row {
	return Row{[]Cell{cell1(s)}}
}
