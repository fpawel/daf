package party

import (
	"github.com/ansel1/merry"
	"github.com/fpawel/comm/modbus"
	"github.com/fpawel/daf/internal/cfg"
	"github.com/fpawel/daf/internal/data"
)

type Product struct {
	data.Product
	Place   int
	Addr    modbus.Addr
	Checked bool
}

func (p Product) WrapError(err error) error {
	if err == nil {
		return err
	}
	return merry.Appendf(err, "место стенда %d", p.Place).
		Appendf("серийный номер %d", p.Serial).
		Appendf("номер продукта %d", p.ProductID)
}

func CheckedProducts() (products []Product) {
	for _, x := range Products() {
		if x.Checked {
			products = append(products, x)
		}
	}
	return
}

//func ProductAtPlace(place int) (p Product) {
//	p.Place = place
//	c := cfg.GetConfig()
//	if place < len(c.Network) {
//		p.Checked = c.Network[place].Checked
//		p.Addr = c.Network[place].Addr
//	}
//
//	if err := data.DB.Get(&p, `
//SELECT product_id, serial
//FROM product
//WHERE party_id = (SELECT party_id FROM last_party)
//ORDER BY product_id
//LIMIT ?,1`, place); err != nil && err != sql.ErrNoRows {
//		panic(err)
//	}
//
//	return
//}

func Products() (products []Product) {
	if err := data.DB.Select(&products, `
SELECT product_id, serial FROM product 
WHERE party_id = (SELECT party_id FROM last_party) 
ORDER BY product_id`); err != nil {
		panic(err)
	}
	c := cfg.GetConfig()
	for i := range products {
		products[i].Place = i
		if i < len(c.Network) {
			products[i].Checked = c.Network[i].Checked
			products[i].Addr = c.Network[i].Addr
		}
	}
	return
}
