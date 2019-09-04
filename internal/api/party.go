package api

import (
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/party"
	"strconv"
)

type PartySvc struct{}

func (_ *PartySvc) NewParty(_ struct{}, _ *struct{}) error {
	data.DB.MustExec(`INSERT INTO party DEFAULT VALUES`)
	data.DB.MustExec(`INSERT INTO product(party_id, serial) VALUES ( (SELECT last_party.party_id FROM last_party), 1 )`)
	return nil
}

func (x *PartySvc) Products(_ struct{}, r *[]party.Product) error {
	*r = party.Products()
	if *r == nil {
		*r = []party.Product{}
	}
	return nil
}

func (_ *PartySvc) Party(_ struct{}, r *data.Party) error {
	*r = data.LastParty()
	return nil
}

func (_ *PartySvc) DeleteProduct(productID [1]int64, r *[]party.Product) error {
	data.DB.MustExec(`DELETE FROM product WHERE product_id = ?`, productID[0])
	*r = party.Products()
	return nil
}

func (_ *PartySvc) AddNewProduct(_ struct{}, r *[]party.Product) error {
	products := party.Products()
	addresses := make(map[modbus.Addr]struct{})
	serials := make(map[int]struct{})
	a := struct{}{}
	for _, x := range products {
		addresses[x.Addr] = a
		serials[x.Serial] = a

	}
	serial, addr := 1, modbus.Addr(1)
	for ; addr <= modbus.Addr(255); addr++ {
		if _, f := addresses[addr]; !f {
			break
		}
	}
	for serial = 1; serial < 100500; serial++ {
		if _, f := serials[serial]; !f {
			break
		}
	}
	data.DB.MustExec(`
INSERT INTO product( party_id, serial) 
VALUES (?,?)`, data.LastParty().PartyID, serial, addr)

	c := cfg.GetConfig()
	c.SetAddrAtPlace(len(products), addr)
	cfg.SetConfig(c)
	*r = party.Products()
	return nil
}

func (_ *PartySvc) SetProductSerial(x struct {
	ProductID int64
	SerialStr string
}, _ *struct{}) error {

	serial, err := strconv.Atoi(x.SerialStr)
	if err != nil {
		return err
	}
	_, err = data.DB.Exec(`UPDATE product SET serial = ? WHERE product_id = ?`, serial, x.ProductID)
	if err != nil {
		return err
	}
	return nil
}
