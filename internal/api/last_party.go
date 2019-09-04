package api

import (
	"github.com/fpawel/daf/internal/data"
	"github.com/fpawel/daf/internal/party"
)

type LastPartySvc struct{}

func (x *LastPartySvc) Products(_ struct{}, r *[]party.Product) error {
	*r = party.Products()
	if *r == nil {
		*r = []party.Product{}
	}
	return nil
}

func (_ *LastPartySvc) Party(_ struct{}, r *data.Party) error {
	*r = data.LastParty()
	return nil
}
