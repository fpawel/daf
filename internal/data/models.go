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
	ProductType     int     `db:"product_type" yaml:"product_type"`
	Component       int     `db:"component" yaml:"component"`
	C1              float64 `db:"c1" yaml:"с1"`
	C2              float64 `db:"c2" yaml:"с2"`
	C3              float64 `db:"c3" yaml:"с3"`
	C4              float64 `db:"c4" yaml:"с4"`
	AbsErrorLimit1  float64 `db:"abs_error_limit1" yaml:"abs_error_limit1"`
	AbsErrorLimit2  float64 `db:"abs_error_limit2" yaml:"abs_error_limit2"`
	AbsErrorLimit3  float64 `db:"abs_error_limit3" yaml:"abs_error_limit3"`
	AbsErrorLimit4  float64 `db:"abs_error_limit4" yaml:"abs_error_limit4"`
	VariationLimit3 float64 `db:"variation_limit3" yaml:"variation_limit3"`
}

type Product struct {
	ProductID  int64   `db:"product_id"`
	Serial     int     `db:"serial"`
	ScaleBegin float64 `db:"scale_begin" yaml:"scale_begin"`
	ScaleEnd   float64 `db:"scale_end" yaml:"scale_end"`
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
