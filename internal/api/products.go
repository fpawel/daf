package api

import (
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/report"
	"strconv"
	"strings"
	"time"
)

type ProductsSvc struct{}

func (_ *ProductsSvc) YearsMonths(_ struct{}, r *[]types.YearMonth) error {
	if err := data.DB.Select(r, `
SELECT DISTINCT 
	cast(strftime('%Y', created_at, 'localtime') AS INT) AS year,
    cast(strftime('%m', created_at, 'localtime') AS INT) AS month 
FROM product
INNER JOIN party ON product.party_id = party.party_id
ORDER BY year DESC, month DESC`); err != nil {
		panic(err)
	}
	return nil
}

type ProductInfo struct {
	ProductID int64 `db:"product_id"`
	PartyID   int64 `db:"party_id"`
	Serial    int   `db:"serial"`
	Day       int   `db:"day"`
	Hour      int   `db:"hour"`
	Minute    int   `db:"minute"`
}

func (_ *ProductsSvc) ProductsOfYearMonth(x types.YearMonth, r *[]ProductInfo) error {
	if err := data.DB.Select(r, `
SELECT cast(strftime('%d', created_at, 'localtime') AS INT) AS day,
       cast(strftime('%H', created_at, 'localtime') AS INT) AS hour,
       cast(strftime('%M', created_at, 'localtime') AS INT) AS minute, 
       product.party_id,
       product_id,
       serial       
FROM product
INNER JOIN party on product.party_id = party.party_id
WHERE cast(strftime('%Y', created_at, 'localtime') AS INT) = ?
  AND cast(strftime('%m', created_at, 'localtime') AS INT) = ?
ORDER BY created_at`, x.Year, x.Month); err != nil {
		panic(err)
	}
	return nil
}

type ProductPassport struct {
	T1, T2    report.Table
	PartyID   int64     `db:"party_id"`
	Serial    int       `db:"serial"`
	CreatedAt time.Time `db:"created_at"`
}

func (_ *ProductsSvc) ProductPassport(x [1]int64, r *ProductPassport) error {
	if err := report.IndividualPassport1(x[0], &r.T1); err != nil {
		return err
	}
	if err := report.IndividualPassport2(x[0], &r.T2); err != nil {
		return err
	}
	return data.DB.Get(r, `
SELECT party.party_id, serial, created_at  FROM product
INNER JOIN party ON product.party_id = party.party_id
WHERE product_id = ?`, x[0])
}

func (_ *ProductsSvc) PartyProducts(x [1]int64, r *[]data.Product) error {
	*r = data.Products(x[0])
	if *r == nil {
		*r = []data.Product{}
	}
	return nil
}

func (_ *ProductsSvc) DeleteEntries(x struct{ Entries []int64 }, _ *struct{}) error {
	var xs []string
	for _, id := range x.Entries {
		xs = append(xs, strconv.FormatInt(id, 10))
	}
	s := "(" + strings.Join(xs, ",") + ")"
	data.DB.MustExec(`DELETE FROM product_entry WHERE product_entry_id IN ` + s)
	return nil
}
