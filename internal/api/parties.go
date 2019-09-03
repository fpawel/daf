package api

import (
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/data"
	"time"
)

type PartiesSvc struct{}

func (_ *PartiesSvc) YearsMonths(_ struct{}, r *[]types.YearMonth) error {
	if err := data.DB.Select(r, `
SELECT DISTINCT 
	cast(strftime('%Y', created_at) AS INT) AS year,
    cast(strftime('%m', created_at) AS INT) AS month 
FROM party 
ORDER BY year DESC, month DESC`); err != nil {
		panic(err)
	}
	return nil
}

type PartyCatalogue struct {
	PartyID     int64  `db:"party_id"`
	ProductType string `db:"product_type"`
	Last        bool   `db:"last"`
	Day         int    `db:"day"`
	Hour        int    `db:"hour"`
	Minute      int    `db:"minute"`
}

func (_ *PartiesSvc) PartiesOfYearMonth(x types.YearMonth, r *[]PartyCatalogue) error {
	if err := data.DB.Select(r, `
SELECT cast(strftime('%d', created_at) AS INT) AS day,
       cast(strftime('%H', created_at) AS INTEGER) AS hour,
       cast(strftime('%M', created_at) AS INTEGER) AS minute, 
       product_type,
       party_id,  
       party_id = (SELECT party_id FROM last_party) AS last
FROM party
WHERE cast(strftime('%Y', created_at) AS INT) = ?
  AND cast(strftime('%m', created_at) AS INT) = ?
ORDER BY created_at`, x.Year, x.Month); err != nil {
		panic(err)
	}
	return nil
}

func (_ *PartiesSvc) PartyProductsValues(x [2]int64, r *mil82.Table) error {
	*r = mil82.NewParty(x[0]).Report(modbus.Var(x[1]))
	return nil
}

func (_ *PartiesSvc) PartyProducts(x [1]int64, r *[]data.Product) error {
	*r = data.Products(x[0])
	if *r == nil {
		*r = []data.Product{}
	}
	return nil
}

func (_ *PartiesSvc) NewParty(_ struct{}, _ *struct{}) error {
	data.DB.MustExec(`INSERT INTO party(created_at) VALUES(?)`, time.Now())
	data.DB.MustExec(`INSERT INTO product(party_id, serial, addr) VALUES ( (SELECT last_party.party_id FROM last_party), 1, 1)`)
	return nil
}
