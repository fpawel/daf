package api

import (
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/last_party"
	"strconv"
)

type LastPartySvc struct{}

func (_ *LastPartySvc) Party(_ struct{}, r *data.Party) error {
	*r = data.LastParty()
	return nil
}

func (_ *LastPartySvc) SetPartySettings(x struct{ A data.PartySettings }, _ *struct{}) error {
	data.DB.MustExec(`
UPDATE party SET product_type = ?, c1 = ?, c2 = ?, c3 = ?, c4 = ?
WHERE party_id = (SELECT party_id FROM last_party)`,
		x.A.ProductType, x.A.C1, x.A.C2, x.A.C3, x.A.C4)
	return nil
}

func (_ *LastPartySvc) SetProductSerial(x struct {
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

func (_ *LastPartySvc) SetProductAddr(x struct {
	ProductID int64
	AddrStr   string
}, _ *struct{}) error {
	addr, err := strconv.Atoi(x.AddrStr)
	if err != nil {
		return err
	}
	_, err = data.DB.Exec(`UPDATE product SET addr = ? WHERE product_id = ?`, addr, x.ProductID)
	if err != nil {
		return err
	}
	return nil
}

func (_ *LastPartySvc) DeleteProduct(productID [1]int64, r *[]last_party.LastPartyProduct) error {
	data.DB.MustExec(`DELETE FROM product WHERE product_id = ?`, productID[0])
	*r = last_party.Products()
	return nil
}

func (_ *LastPartySvc) AddNewProduct(_ struct{}, r *[]last_party.LastPartyProduct) error {
	products := last_party.Products()
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
INSERT INTO product( party_id, serial, addr) 
VALUES (?,?,?)`, data.LastParty().PartyID, serial, addr)

	*r = last_party.Products()
	return nil
}

func (_ *LastPartySvc) Products(_ struct{}, r *[]last_party.LastPartyProduct) error {
	*r = last_party.Products()
	if *r == nil {
		*r = []last_party.LastPartyProduct{}
	}
	return nil
}

func (_ *LastPartySvc) Products1(_ struct{}, r *[]data.Product) error {
	*r = data.Products(data.LastParty().PartyID)
	if *r == nil {
		*r = []data.Product{}
	}
	return nil
}

func (_ *LastPartySvc) ProductsValues(x [1]int64, r *mil82.Table) error {
	*r = mil82.NewParty(data.LastParty().PartyID).Report(modbus.Var(x[0]))
	return nil
}
