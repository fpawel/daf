package daf

import (
	"fmt"
	"github.com/fpawel/mil82/internal/data"
	"math"
	"math/rand"
	"strings"
	"testing"
	"time"
)

func TestCreateDB(t *testing.T) {
	for y := 2018; y <= 2019; y++ {
		for m := time.Month(3); m <= 5; m++ {
			for d := 1; d <= 3; d++ {
				fmt.Printf("%02d:%02d:%02d\n", d, m, y)
				t := time.Date(y, m, d, 16, 12, 0, 0, time.Now().Location())
				r, err := data.DB.Exec(`INSERT INTO party (created_at) VALUES (?)`, t)
				if err != nil {
					panic(err)
				}
				partyID, err := r.LastInsertId()
				if err != nil {
					panic(err)
				}
				for i := 0; i < 20; i++ {
					r, err := data.DB.Exec(
						`INSERT INTO product (party_id, serial, addr) VALUES (?, ?, ?)`,
						partyID, i+100, i+1)
					if err != nil {
						panic(err)
					}
					productID, err := r.LastInsertId()
					if err != nil {
						panic(err)
					}
					var xs []string
					for _, work := range Works {
						for gas := Gas1; gas <= Gas4; gas++ {
							for _, temp := range work.Temps() {
								for _, Var := range Vars {
									value := rand.Float64() * 100
									value = math.Round(value*100) / 100
									xs = append(xs, fmt.Sprintf("(%d, %d, '%s', %d, '%s', %v)",
										productID, Var, work, gas, temp, value))
								}
							}
						}
					}
					qStr := "INSERT INTO product_value (product_id, var, work, gas, temp, value) VALUES " +
						strings.Join(xs, ", ")
					//fmt.Println(qStr)
					data.DB.MustExec(qStr)
				}
			}
		}
	}
}
