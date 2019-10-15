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
	ProductID  int64   `db:"product_id"`
	Serial     int     `db:"serial"`
	ScaleBegin float64 `db:"scale_begin" toml:"scale_begin" comment:"нижняя граница диапазона измерений"`
	ScaleEnd   float64 `db:"scale_end" toml:"scale_end" comment:"верхняя граница диапазона измерений"`
}

type Test struct {
	Nominal            float64
	AbsErrorLimit      float64
	MustThr1, MustThr2 bool
}

func (x Test) ConcentrationOk(c float64) bool {
	return math.Abs(c-x.Nominal) < x.AbsErrorLimit
}

func (x Test) CurrentOk(i, scaleBegin, scaleEnd float64) bool {
	return x.ConcentrationOk(CurrentToConcentration(i, scaleBegin, scaleEnd))
}

func CurrentToConcentration(i, scaleBegin, scaleEnd float64) float64 {
	return math.Round((i-4.)*(scaleEnd-scaleBegin)/16.*1000) / 1000
}

func (p Party) Tests() []Test {
	//MustThr1 =   []bool{false, false, true,  true, true,  false}
	//	MustThr2 = []bool{false, false, false, true, false, false}
	return []Test{
		{p.C1, p.AbsErrorLimit1, false, false},
		{p.C2, p.AbsErrorLimit2, false, false},
		{p.C3, p.AbsErrorLimit3, true, false},
		{p.C4, p.AbsErrorLimit4, true, true},
		{p.C3, p.AbsErrorLimit3, true, false},
		{p.C1, p.AbsErrorLimit1, false, false},
	}
}
