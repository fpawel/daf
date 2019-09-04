package report

import (
	"fmt"
	"github.com/fpawel/daf/internal/data"
)

type Table = []Row

type Row = []Cell

type Cell = string

func Party(partyID int64) Table {

	party := data.GetParty(partyID)
	products := data.Products(partyID)

	titles := []string{
		"Номер", "День", "Загрузка", "Зав.номер",
	}

	r1 := append([]Row{titles}, make([]Row, len(products))...)
	hasCols := make(map[int]struct{})
	for i, p := range products {
		r := make(Row, len(titles))
		r[0] = fmt.Sprintf("%d", p.ProductID)
		r[1] = fmt.Sprintf("%d", party.CreatedAt.Day())
		r[2] = fmt.Sprintf("%d", party.PartyID)
		r[3] = fmt.Sprintf("%d", p.Serial)
		for j := range titles {
			if len(r[j]) > 0 {
				hasCols[j] = struct{}{}
			}
		}
		r1[i+1] = r
	}

	var r2 Table
	for _, row1 := range r1 {
		var row2 Row
		for col := range r1[0] {
			if _, f := hasCols[col]; f {
				row2 = append(row2, row1[col])
			}
		}
		r2 = append(r2, row2)
	}
	return r2
}
