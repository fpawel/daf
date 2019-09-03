package daf

import "github.com/fpawel/mil82/internal/data"

type Party struct {
	data.Party
	Products    []data.Product
	ProductType ProductType
}

func NewParty(partyID int64) Party {
	x := Party{
		Party: data.GetParty(partyID),
	}
	if err := data.DB.Select(&x.Products, `SELECT product_id, addr, serial FROM product WHERE party_id = ?`, partyID); err != nil {
		panic(err)
	}
	x.ProductType = ProductTypeByName(x.Party.ProductType)
	return x
}

func (x Party) concentration(gas Gas) float64 {
	switch gas {
	case Gas1:
		return x.C1
	case Gas2:
		return x.C2
	case Gas3:
		return x.C3
	case Gas4:
		return x.C4
	default:
		panic(gas)
	}
}
