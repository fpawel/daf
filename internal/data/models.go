package data

import (
	"github.com/fpawel/comm/modbus"
	"time"
)

type Party struct {
	PartySettings
	PartyID   int64     `db:"party_id"`
	CreatedAt time.Time `db:"created_at"`
}

type PartySettings struct {
	ProductType string  `db:"product_type"`
	C1          float64 `db:"c1"`
	C2          float64 `db:"c2"`
	C3          float64 `db:"c3"`
	C4          float64 `db:"c4"`
}

type Product struct {
	ProductID int64       `db:"product_id"`
	Serial    int         `db:"serial"`
	Addr      modbus.Addr `db:"addr"`
}

type ProductValue struct {
	ProductID int64   `db:"product_id"`
	Work      string  `db:"work"`
	Var       int     `db:"var"`
	Gas       string  `db:"gas"`
	Temp      string  `db:"temp"`
	Value     float64 `db:"value"`
}

type ProductCoefficient struct {
	ProductID   int64   `db:"product_id"`
	Coefficient int     `db:"coefficient"`
	Value       float64 `db:"value"`
}
