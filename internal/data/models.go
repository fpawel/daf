package data

import (
	"time"
)

type Party struct {
	PartySettings
	PartyID   int64     `db:"party_id"`
	CreatedAt time.Time `db:"created_at"`
}

type PartySettings struct {
	ProductType int     `db:"product_type" toml:"product_type" comment:"код исполнения ДАФ-М согласно ТУ"`
	Component   int     `db:"component" toml:"component" comment:"код измеряемого компонента согласно ТУ"`
	C1          float64 `db:"c1" toml:"с1" comment:"концентрация ГСО-ПГС1"`
	C2          float64 `db:"c2" toml:"с2" comment:"концентрация ГСО-ПГС2"`
	C3          float64 `db:"c3" toml:"с3" comment:"концентрация ГСО-ПГС3"`
	C4          float64 `db:"c4" toml:"с4" comment:"концентрация ГСО-ПГС4"`
	Scale       float64 `db:"scale" toml:"scale" comment:"шкала"`
	AbsErrRng   float64 `db:"abs_err_rng" toml:"abs_err_rng" comment:"диапазон абсолютной погрешности"`
	AbsErrLim   float64 `db:"abs_err_lim" toml:"abs_err_lim" comment:"предел абсолютной погрешности"`
	RelErrLim   float64 `db:"rel_err_lim" toml:"abs_err_lim" comment:"предел относительной погрешности"`
	Thr1Prod    float64 `db:"thr1_prod" toml:"thr1_prod" comment:"значение ПОРОГ1 для выпуска в эксплуатацию"`
	Thr2Prod    float64 `db:"thr2_prod" toml:"thr2_prod" comment:"значение ПОРОГ2 для выпуска в эксплуатацию"`
	Thr1Test    float64 `db:"thr1_test" toml:"thr1_test" comment:"значение ПОРОГ1 для настройки"`
	Thr2Test    float64 `db:"thr2_test" toml:"thr2_test" comment:"значение ПОРОГ2 настройки"`
}

type Product struct {
	ProductID int64 `db:"product_id"`
	Serial    int   `db:"serial"`
}
