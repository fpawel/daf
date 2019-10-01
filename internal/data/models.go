package data

import (
	"math"
	"time"
)

type Party struct {
	PartyConfig
	PartyID   int64     `db:"party_id"`
	CreatedAt time.Time `db:"created_at"`
}

type PartyConfig struct {
	ProductType     int     `db:"product_type" toml:"product_type" comment:"код исполнения ДАФ-М согласно ТУ"`
	Component       int     `db:"component" toml:"component" comment:"код измеряемого компонента согласно ТУ"`
	ScaleBegin      float64 `db:"scale_begin" toml:"scale_begin" comment:"нижняя граница диапазона измерений"`
	ScaleEnd        float64 `db:"scale_end" toml:"scale_end" comment:"верхняя граница диапазона измерений"`
	C1              float64 `db:"c1" toml:"с1" comment:"концентрация ГСО-ПГС1"`
	C2              float64 `db:"c2" toml:"с2" comment:"концентрация ГСО-ПГС2"`
	C3              float64 `db:"c3" toml:"с3" comment:"концентрация ГСО-ПГС3"`
	C4              float64 `db:"c4" toml:"с4" comment:"концентрация ГСО-ПГС4"`
	AbsErrorLimit1  float64 `db:"abs_error_limit1" toml:"abs_error_limit1" comment:"предел допускаемой основной погрешности при подаче ГСО-ПГС1"`
	AbsErrorLimit2  float64 `db:"abs_error_limit2" toml:"abs_error_limit2" comment:"предел допускаемой основной погрешности при подаче ГСО-ПГС2"`
	AbsErrorLimit3  float64 `db:"abs_error_limit3" toml:"abs_error_limit3" comment:"предел допускаемой основной погрешности при подаче ГСО-ПГС3"`
	AbsErrorLimit4  float64 `db:"abs_error_limit4" toml:"abs_error_limit4" comment:"предел допускаемой основной погрешности при подаче ГСО-ПГС4"`
	VariationLimit3 float64 `db:"variation_limit3" toml:"variation_limit3" comment:"предел вариации показаний при ГСО-ПГС3"`
}

type Product struct {
	ProductID int64 `db:"product_id"`
	Serial    int   `db:"serial"`
}

func (p Party) Ci(i float64) float64 {
	return (i - 4.) * (p.ScaleEnd - p.ScaleBegin) / 16.
}

func (p Party) CnTest(test int) float64 {
	return ([]float64{p.C1, p.C2, p.C3, p.C4, p.C3, p.C1})[test]
}

func (p Party) TestConcentrationOk(test int, c float64) bool {
	return math.Abs(c-p.CnTest(test)) < p.AbsErrLimitTest(test)
}

func (p Party) TestOutputCurrentOk(test int, i float64) bool {
	return math.Abs(p.Ci(i)-p.CnTest(test)) < p.AbsErrLimitTest(test)
}

func (p Party) AbsErrLimitTest(test int) float64 {
	return ([]float64{
		p.AbsErrorLimit1,
		p.AbsErrorLimit2,
		p.AbsErrorLimit3,
		p.AbsErrorLimit4,
		p.AbsErrorLimit3,
		p.AbsErrorLimit1,
	})[test]
}
