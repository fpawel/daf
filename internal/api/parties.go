package api

import (
	"fmt"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/api/types"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/report"
	"github.com/pelletier/go-toml"
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

func (_ *PartiesSvc) ReportParty(x [1]int64, r *report.Table) error {
	*r = report.Party(x[0])
	return nil
}

func (_ *PartiesSvc) PartyProducts(x [1]int64, r *[]data.Product) error {
	*r = data.Products(x[0])
	if *r == nil {
		*r = []data.Product{}
	}
	return nil
}

func (x *PartiesSvc) NewPartyTemplate(_ struct{}, r *string) error {
	return tomlStringify(r, addrSerials{
		{
			Addr:   1,
			Serial: 100,
		},
		{
			Addr:   2,
			Serial: 100,
		},
		{
			Addr:   3,
			Serial: 100,
		},
	})
}

func (x *PartiesSvc) CreateNewParty(s [1]string, _ *struct{}) error {
	var p addrSerials
	if err := toml.Unmarshal([]byte(s[0]), &p); err != nil {
		return err
	}
	if _, err := data.DB.Exec(`BEGIN TRANSACTION; INSERT INTO party DEFAULT VALUES;`); err != nil {
		return err
	}
	partyID := data.LastParty().PartyID
	c := cfg.GetConfig()
	for i, x := range p {
		if _, err := data.DB.Exec(`INSERT INTO product(party_id, serial) VALUES ( ?, ?)`, partyID, x.Serial); err != nil {
			data.DB.MustExec(`ROLLBACK;`)
			return fmt.Errorf("serial=%d: %v", x.Serial, err)
		}
		if i < len(c.Network) {
			c.Network[i].Addr = x.Addr
		}
	}
	data.DB.MustExec(`COMMIT;`)
	cfg.SetConfig(c)
	return nil
}

func tomlStringify(r *string, x interface{}) error {
	b, err := toml.Marshal(x)
	if err != nil {
		return err
	}
	*r = string(b)
	return nil
}

type addrSerial struct {
	Addr   modbus.Addr `toml:"addr" comment:"адрес прибора MODBUS"`
	Serial int         `toml:"serial" comment:"серийный номер прибора"`
}
type addrSerials = []addrSerial
