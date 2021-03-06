package data

import (
	"database/sql"
	"github.com/fpawel/daf/internal/pkg/must"
	"github.com/jmoiron/sqlx"
	"os"
	"path/filepath"
)

//go:generate go run github.com/fpawel/gotools/cmd/sqlstr/...

//func OpenDev() {
//	Open(filepath.Join(os.Getenv("GOPATH"), "src", "github.com", "fpawel", "daf", "build", "daf.sqlite"))
//}

func OpenProd() {
	Open(filepath.Join(filepath.Dir(os.Args[0]), "daf.sqlite"))
}

func Open(filename string) {
	DB = must.OpenSqliteDBx(filename)
	DB.MustExec(SQLCreate)
}

var (
	DB *sqlx.DB
)

func GetPartyByProductID(productID int64) (party Party) {
	if err := DB.Get(&party, `
SELECT party.* 
FROM product
INNER JOIN party USING (party_id)
WHERE product_id=?`, productID); err != nil {
		panic(err)
	}
	return
}

func LastParty() (party Party) {
	err := DB.Get(&party, `SELECT * FROM last_party`)
	if err == sql.ErrNoRows {
		DB.MustExec(`
INSERT INTO party DEFAULT VALUES;
INSERT INTO product(party_id, serial) VALUES ((SELECT party_id FROM last_party), 1);
`)
		err = DB.Get(&party, `SELECT * FROM last_party`)
	}
	if err != nil {
		panic(err)
	}
	return
}

func Products(partyID int64) (products []Product) {
	if err := DB.Select(&products, `
SELECT product_id, serial  FROM product 
WHERE party_id = ? 
ORDER BY product_id`, partyID); err != nil {
		panic(err)
	}
	return
}

func MustSetProductScaleEnd(productID int64, scaleEnd float64) {
	DB.MustExec(`UPDATE product SET scale_end = ? WHERE product_id = ?`,
		scaleEnd, productID)
}

func MustSetProductScaleBegin0(productID int64) {
	DB.MustExec(`UPDATE product SET scale_begin = 0 WHERE product_id = ?`, productID)
}
